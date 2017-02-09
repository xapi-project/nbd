module TestPacket = struct
        type id = int

        type request_hdr = {
		req_id : int;
		req_payload : string;
	} with rpc

        type request_body = unit

        type response_hdr = {
		res_id : int option;
		res_payload : string;
	} with rpc

        type response_body = unit

        type seq = Request of request_hdr | Response of response_hdr

        type transport = {
                recv_cond : unit Lwt_condition.t;
                mutex : Lwt_mutex.t;
                recv_queue : response_hdr Queue.t;
                mutable seq : seq list;
        }

	let record_sequence = ref true
                
        let recv_hdr t = 
            Lwt_mutex.with_lock t.mutex (fun () ->
                lwt () = while_lwt Queue.is_empty t.recv_queue do
                                Lwt_condition.wait ~mutex:t.mutex t.recv_cond
                done in
                let res = Queue.pop t.recv_queue in
                if !record_sequence then t.seq <- (Response res) :: t.seq;
                Lwt.return (res.res_id, res))

        let recv_body t _ _ = 
                Lwt.return ()

        let send_one t x _ =
                Lwt_mutex.with_lock t.mutex (fun () ->
                        if !record_sequence then t.seq <- (Request x) :: t.seq; Lwt.return ())

        let id_of_request r =
                r.req_id

        let handle_unrequested_packet t p =
		Printf.printf "Handling unexpected packet!\n%!";
		if p.res_payload = "exception"
		then (Printf.printf "Raising exception in mux!%!"; Lwt.fail (Failure "requested exception"))
		else Lwt.return ()

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

module T = Nbd_lwt_mux.Mux(TestPacket) 

let test () =
    let transport = TestPacket.create () in
    lwt client = T.create transport in
    let open TestPacket in
    let p1 = { req_id = 1; req_payload = "p1" } in
    let p2 = { req_id = 2; req_payload = "p2" } in
    let r1 = { res_id = Some 1; res_payload = "r1" } in
    let r2 = { res_id = Some 2; res_payload = "r2" } in

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

    TestPacket.record_sequence := false;
    
    let before = Gc.stat () in
    let rec megaqueue n =
       if n=100000 then Lwt.return () else
       let t1 = T.rpc p1 () client in
       lwt () = TestPacket.queue_response r1 transport in
       lwt (test_r1,()) = t1 in
       if n mod 10000 = 0 then begin
	  Gc.compact ();
	  let test = Gc.stat () in
	  if test.Gc.live_words > 10000 then failwith "Memory leak!"
       end;
       megaqueue (n+1)
    in
    lwt () = megaqueue 0 in
    lwt () = TestPacket.queue_response { res_id=None; res_payload="exception" } transport in
    let t1 = T.rpc p1 () client in
    lwt () = TestPacket.queue_response r2 transport in
    lwt ok = try_lwt lwt (t1_test,()) = t1 in Lwt.return false with e -> Printf.printf "Exception: %s\n%!" (Printexc.to_string e); Lwt.return true in
    Printf.printf "%s\n" (if ok then "OK!" else "Not OK!");


    List.iter (function | TestPacket.Request req -> Printf.printf "Request: %s\n" (Jsonrpc.to_string (rpc_of_request_hdr req))
                | TestPacket.Response res -> Printf.printf "  Response: %s\n" (Jsonrpc.to_string (rpc_of_response_hdr res))) transport.TestPacket.seq;

    Lwt.return ()


let _ =
    Lwt.ignore_result (test ())

