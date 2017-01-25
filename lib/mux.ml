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

(* Lwt connection multiplexer *)
open Lwt

module type RPC = sig
  type transport
  type id
  type request_hdr
  type request_body
  type response_hdr
  type response_body

  val recv_hdr : transport -> (id option * response_hdr) Lwt.t
  val recv_body : transport -> request_hdr -> response_hdr -> response_body -> (unit, Protocol.Error.t) Result.result Lwt.t
  val send_one : transport -> request_hdr -> request_body -> unit Lwt.t
  val id_of_request : request_hdr -> id
  val handle_unrequested_packet : transport -> response_hdr -> unit Lwt.t
end


module Make = functor (R : RPC) -> struct
  exception Unexpected_id of R.id
  exception Shutdown

  type client = {
    transport : R.transport;
    outgoing_mutex: Lwt_mutex.t;
    id_to_wakeup : (R.id, R.request_hdr * ((unit, Protocol.Error.t) Result.result Lwt.u) * R.response_body) Hashtbl.t;
    mutable dispatcher_thread : unit Lwt.t;
    mutable dispatcher_shutting_down : bool;
  }

  let rec dispatcher t =
    Lwt.catch
    (fun () ->
      R.recv_hdr t.transport
      >>= fun (id, pkt) ->
      match id with
      | None -> R.handle_unrequested_packet t.transport pkt
      | Some id ->
        if not(Hashtbl.mem t.id_to_wakeup id)
        then fail (Unexpected_id id)
        else begin
          let request_hdr, waker, response_body = Hashtbl.find t.id_to_wakeup id in
          R.recv_body t.transport request_hdr pkt response_body
          >>= fun response ->
          Lwt.wakeup waker response;
          Hashtbl.remove t.id_to_wakeup id;
          dispatcher t
        end
    ) (fun e ->
      t.dispatcher_shutting_down <- true;
      Hashtbl.iter (fun _ (_,u, _) -> Lwt.wakeup_later_exn u e) t.id_to_wakeup;
      fail e)

let rpc req_hdr req_body response_body t =
  let sleeper, waker = Lwt.wait () in
  if t.dispatcher_shutting_down
  then fail Shutdown
  else begin
    let id = R.id_of_request req_hdr in
    Hashtbl.add t.id_to_wakeup id (req_hdr, waker, response_body);
    Lwt_mutex.with_lock t.outgoing_mutex
      (fun () -> R.send_one t.transport req_hdr req_body)
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


(*
module TestPacket = struct
	type id = int
	type request_hdr = Lwt_mux_test.request
	type request_body = unit
	type response_hdr = Lwt_mux_test.response
	type response_body = unit

	type seq = Request of request_hdr | Response of response_hdr

	type transport = {
		recv_cond : unit Lwt_condition.t;
		mutex : Lwt_mutex.t;
		recv_queue : response_hdr Queue.t;
		mutable seq : seq list;
	}

	let recv_hdr t =
		Lwt_mutex.with_lock t.mutex (fun () ->
			lwt () = while_lwt Queue.is_empty t.recv_queue do
				Lwt_condition.wait ~mutex:t.mutex t.recv_cond
                done in
            let res = Queue.pop t.recv_queue in
            t.seq <- (Response res) :: t.seq;
            Lwt.return (Some res.Lwt_mux_test.res_id, res))

	let recv_body t _ _ =
		Lwt.return ()

	let send_one t x _ =
		Lwt_mutex.with_lock t.mutex (fun () ->
			t.seq <- (Request x) :: t.seq; Lwt.return ())

	let id_of_request r =
		r.Lwt_mux_test.req_id

	let handle_unrequested_packet t p =
		Lwt.return ()

	let create () =
		{ recv_cond = Lwt_condition.create ();
		  mutex = Lwt_mutex.create ();
		  recv_queue = Queue.create ();
		  seq = []; }

	let queue_response res t =
		Lwt_mutex.with_lock t.mutex (fun () ->
			Queue.push res t.recv_queue;
			Lwt_condition.broadcast t.recv_cond ();
			Lwt.return ())
end



module T = Make(TestPacket)

let test () =
	let transport = TestPacket.create () in
	lwt client = T.create transport in
	let open Lwt_mux_test in
	let p1 = { req_id = 1; req_payload = "p1" } in
	let p2 = { req_id = 2; req_payload = "p2" } in
	let r1 = { res_id = 1; res_payload = "r1" } in
	let r2 = { res_id = 2; res_payload = "r2" } in

	let t1 = T.rpc p1 () client in
	lwt () = TestPacket.queue_response r1 transport in
    lwt (test_r1,()) = t1 in
    (if test_r1 = r1 then Printf.printf "OK!\n" else Printf.printf "Not OK!\n");

    let t1 = T.rpc p1 () client in
    let t2 = T.rpc p2 () client in
    lwt () = TestPacket.queue_response r1 transport in
    lwt () = TestPacket.queue_response r2 transport in
    lwt (test_r1,()) = t1 and (test_r2,()) = t2 in
    (if test_r1 = r1 && test_r2 = r2 then Printf.printf "OK!\n" else Printf.printf "Not OK!\n");

    let t1 = T.rpc p1 () client in
    let t2 = T.rpc p2 () client in
    lwt () = TestPacket.queue_response r2 transport in
    lwt () = TestPacket.queue_response r1 transport in
    lwt (test_r1,()) = t1 and (test_r2,()) = t2 in
    (if test_r1 = r1 && test_r2 = r2 then Printf.printf "OK!\n" else Printf.printf "Not OK!\n");

    let t1 = T.rpc p1 () client in
    lwt () = TestPacket.queue_response r2 transport in
    lwt ok = try_lwt lwt (t1_test,()) = t1 in Lwt.return false with e -> Lwt.return true in
    Printf.printf "%s\n" (if ok then "OK!" else "Not OK!");

    List.iter (function | TestPacket.Request req -> Printf.printf "Request: %s\n" (Jsonrpc.to_string (rpc_of_request req))
		| TestPacket.Response res -> Printf.printf "  Response: %s\n" (Jsonrpc.to_string (rpc_of_response res))) transport.TestPacket.seq;

    Lwt.return ()


let _ =
	Lwt.ignore_result (test ())
*)
