(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(** Lwt connection multiplexer. Multiplexes between parallel requests from
    multiple clients over a single output channel to a server that may send
    responses out of order. Each request and response carries an [id] that is
    used to match responses to requests. *)

open Lwt.Infix

module type RPC = sig
  type transport
  (** The transport mechanism used to send and receive messages *)

  type id

  type request_hdr
  type response_hdr

  val send_hdr : transport -> request_hdr -> unit Lwt.t
  val recv_hdr : transport -> response_hdr Lwt.t

  val id_of_request : request_hdr -> id

  val id_of_response : response_hdr -> id
  val final_response : response_hdr -> bool
end

module Make (R : RPC) : sig
  type client

  type send_request_body = R.transport -> unit Lwt.t
  type 'a recv_response_body = 'a -> R.transport -> R.response_hdr -> 'a Lwt.t

  val rpc : client -> R.request_hdr -> send_request_body -> 'a recv_response_body -> 'a -> 'a Lwt.t

  val create : R.transport -> client Lwt.t

end = struct
  type send_request_body = R.transport -> unit Lwt.t
  type 'a recv_response_body = 'a -> R.transport -> R.response_hdr -> 'a Lwt.t

  exception Unexpected_id of R.id
  exception Shutdown

  type callback = RecvCallback : 'a Lwt.u * 'a * 'a recv_response_body -> callback

  type client = {
    transport : R.transport;
    outgoing_mutex: Lwt_mutex.t;
    id_to_wakeup : (R.id, callback) Hashtbl.t;
    mutable dispatcher_thread : unit Lwt.t;
    mutable dispatcher_shutting_down : bool;
  }

  let rec dispatcher t =
    let th = Lwt.catch
        (fun () ->
           R.recv_hdr t.transport >>= fun response_hdr ->
           let id = R.id_of_response response_hdr in
           let final = R.final_response response_hdr in
           if not(Hashtbl.mem t.id_to_wakeup id)
           then Lwt.fail (Unexpected_id id)
           else begin
             match Hashtbl.find t.id_to_wakeup id with
             | RecvCallback (waker, res, recv_response_body) ->
               recv_response_body res t.transport response_hdr >>= fun res ->
               if final then begin
                 Hashtbl.remove t.id_to_wakeup id;
                 Lwt.wakeup waker res
               end else begin
                 Hashtbl.replace
                   t.id_to_wakeup
                   id
                   (RecvCallback (waker, res, recv_response_body))
               end;
               Lwt.return ()
           end
        ) (fun e ->
           t.dispatcher_shutting_down <- true;
           Hashtbl.iter (fun _ -> function RecvCallback (u,_,_) -> Lwt.wakeup_later_exn u e) t.id_to_wakeup;
           Lwt.fail e)
    in th >>= fun () -> dispatcher t

  let rpc t request_hdr send_request_body recv_response_body init =
    let sleeper, waker = Lwt.wait () in
    if t.dispatcher_shutting_down
    then Lwt.fail Shutdown
    else begin
      let id = R.id_of_request request_hdr in
      Hashtbl.add t.id_to_wakeup id (RecvCallback (waker, init, recv_response_body));
      Lwt_mutex.with_lock t.outgoing_mutex
        (fun () ->
           R.send_hdr t.transport request_hdr >>= fun () ->
           send_request_body t.transport
        )
      >>= fun () ->
      sleeper
    end

  let create transport =
    let t = {
      transport = transport;
      outgoing_mutex = Lwt_mutex.create ();
      id_to_wakeup = Hashtbl.create 10;
      dispatcher_thread = Lwt.return ();
      dispatcher_shutting_down = false; } in
    t.dispatcher_thread <- dispatcher t;
    Lwt.return t

end
