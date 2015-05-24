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

open Lwt
open Nbd
open Nbd_lwt_common
open Nbd_lwt_channel

type name = string

type 'a t = {
  channel: channel;
  request: Cstruct.t; (* buffer used to read the request headers *)
  reply: Cstruct.t;   (* buffer used to write the response headers *)
  m: Lwt_mutex.t; (* prevents partial message interleaving *)
}

type size = int64

let close t = t.channel.close ()

let make channel =
  let request = Cstruct.create Request.sizeof in
  let reply = Cstruct.create Reply.sizeof in
  let m = Lwt_mutex.create () in
  { channel; request; reply; m }

let negotiate channel size flags =
  let buf = Cstruct.create Announcement.sizeof in
  Announcement.(marshal buf `V1);
  channel.write buf
  >>= fun () ->
  let buf = Cstruct.create (Negotiate.sizeof `V1) in
  Negotiate.(marshal buf (V1 { size; flags }));
  channel.write buf
  >>= fun () ->
  return (make channel)

let negotiate_begin channel ?offer () =
  let buf = Cstruct.create Announcement.sizeof in
  Announcement.(marshal buf `V2);
  channel.write buf
  >>= fun () ->
  let buf = Cstruct.create (Negotiate.sizeof `V2) in
  Negotiate.(marshal buf (V2 [ GlobalFlag.Fixed_newstyle ]));
  channel.write buf
  >>= fun () ->
  let buf = Cstruct.create NegotiateResponse.sizeof in
  channel.read buf
  >>= fun () ->
  let client_flags = NegotiateResponse.unmarshal buf in
  (* Assume that the client supports Fixed_newstyle? *)
  if not (List.mem ClientFlag.Fixed_newstyle client_flags)
  then Printf.fprintf stderr "WARNING: client doesn't report Fixed_newstyle\n%!";
  (* Option negotiation *)
  let req = Cstruct.create OptionRequestHeader.sizeof in
  let res = Cstruct.create OptionResponseHeader.sizeof in
  let rec loop () =
    channel.read req
    >>= fun () ->
    match OptionRequestHeader.unmarshal req with
    | `Error e -> fail e
    | `Ok hdr ->
      let payload = Cstruct.create (Int32.to_int hdr.OptionRequestHeader.length) in
      channel.read payload
      >>= fun () ->
      begin match hdr.OptionRequestHeader.ty with
      | Option.ExportName -> return (Cstruct.to_string payload, make channel)
      | Option.Abort -> fail (Failure "client requested abort")
      | Option.Unknown x ->
        OptionResponseHeader.(marshal res { request_type = Option.Unknown x; response_type = OptionResponse.Unsupported; length = 0l });
        channel.write res
        >>= fun () ->
        loop ()
      | Option.List ->
        begin match offer with
        | None ->
          OptionResponseHeader.(marshal res { request_type = Option.List; response_type = OptionResponse.Policy; length = 0l });
          channel.write res
          >>= fun () ->
          loop ()
        | Some offers ->
          let rec advertise = function
          | [] ->
            OptionResponseHeader.(marshal res { request_type = Option.List; response_type = OptionResponse.Ack; length = 0l });
            channel.write res
          | x :: xs ->
            let l = String.length x in
            OptionResponseHeader.(marshal res { request_type = Option.List; response_type = OptionResponse.Server; length = Int32.of_int l});
            channel.write res
            >>= fun () ->
            let name = Cstruct.create l in
            Cstruct.blit_from_string x 0 name 0 l;
            channel.write name
            >>= fun () ->
            advertise xs in
          advertise offers
          >>= fun () ->
          loop ()
        end
      end in
  loop ()

let negotiate_end t size flags = fail (Failure "unimplemented")

let next t =
  t.channel.read t.request
  >>= fun () ->
  match Request.unmarshal t.request with
  | `Ok r -> return r
  | `Error e -> fail e

let ok t handle payload =
  Lwt_mutex.with_lock t.m
    (fun () ->
      Reply.marshal t.reply { Reply.handle; error = `Ok () };
      t.channel.write t.reply
      >>= fun () ->
      match payload with
      | None -> return ()
      | Some data -> t.channel.write data
    )

let error t handle code =
  Lwt_mutex.with_lock t.m
    (fun () ->
      Reply.marshal t.reply { Reply.handle; error = `Error code };
      t.channel.write t.reply
    )

