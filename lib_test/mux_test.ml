open Lwt.Infix

module TestPacket = struct
  type id = int

  type request_hdr = {
    req_id : int;
    req_payload : bytes;
  } [@@deriving sexp]

  type response_hdr = {
    res_id : int;
    res_payload : bytes;
  } [@@deriving sexp]

  type seq = Request of request_hdr | Response of response_hdr

  type transport = {
    recv_cond : unit Lwt_condition.t;
    mutex : Lwt_mutex.t;
    recv_queue : response_hdr Queue.t;
    mutable seq : seq list;
  }

  let record_sequence = ref true

  let send_hdr t x =
    Lwt_mutex.with_lock t.mutex (fun () ->
        if !record_sequence then t.seq <- (Request x) :: t.seq; Lwt.return ())

  let recv_hdr t =
    Lwt_mutex.with_lock t.mutex (fun () ->
        let loop () =
          if Queue.is_empty t.recv_queue then begin
            Lwt_condition.wait ~mutex:t.mutex t.recv_cond
          end else Lwt.return () in
        loop () >>= fun () ->
        let res = Queue.pop t.recv_queue in
        if !record_sequence then t.seq <- (Response res) :: t.seq;
        Lwt.return res)

  let id_of_request r =
    r.req_id

  let id_of_response r = r.res_id

  let final_response _r = true

  let send_request_body _ = Lwt.return_unit

  let recv_response_body rsp_body _ _ rsp_hdr =
    Bytes.blit rsp_hdr.res_payload 0 rsp_body 0 (Bytes.length rsp_hdr.res_payload);
    Lwt.return_ok ()

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

module T = Nbd.Mux.Make(TestPacket)

(* Some helpful packets for all tests *)
let p1 = TestPacket.{ req_id = 1; req_payload = "p1" |> Bytes.of_string }
let p2 = TestPacket.{ req_id = 2; req_payload = "p2" |> Bytes.of_string }
let r1 = TestPacket.{ res_id = 1; res_payload = "r1" |> Bytes.of_string }
let r2 = TestPacket.{ res_id = 2; res_payload = "r2" |> Bytes.of_string }

let (>>|=) m f =
  (* Check for an `Ok result in an Lwt thread, and fail the
     thread if it's an Error *)
  m >>= function
  | Ok x -> f x
  | Error x -> Lwt.fail_with (Nbd.Protocol.Error.to_string x)


let test_rpc =
  "Basic test of the rpc function",
  `Quick,
  fun () ->
    let t =
      let transport = TestPacket.create () in
      T.create transport >>= fun client ->
      let open TestPacket in
      let response = Bytes.create 2 in
      let t1 = T.rpc client p1 TestPacket.send_request_body (TestPacket.recv_response_body response) (Ok ()) in
      TestPacket.queue_response r1 transport >>= fun () ->
      t1 >>|= fun _ ->
      Lwt.return (response = r1.res_payload)
    in Alcotest.(check bool) "RPC response correct" true (Lwt_main.run t)

let test_multi_rpc =
  "Test queuing of rpc calls in the mux",
  `Quick,
  fun () ->
    let t =
      let transport = TestPacket.create () in
      T.create transport >>= fun client ->
      let open TestPacket in
      let response1 = Bytes.create 2 in
      let response2 = Bytes.create 2 in
      let t1 = T.rpc client p1 TestPacket.send_request_body (TestPacket.recv_response_body response1) (Ok ()) in
      let t2 = T.rpc client p2 TestPacket.send_request_body (TestPacket.recv_response_body response2) (Ok ()) in
      TestPacket.queue_response r1 transport >>= fun () ->
      TestPacket.queue_response r2 transport >>= fun () ->
      t1 >>|= fun _ -> t2 >>|= fun _ ->
      Lwt.return (response1 = r1.res_payload && response2 = r2.res_payload)
    in Alcotest.(check bool) "Both responses correct" true (Lwt_main.run t)

let test_out_of_order_responses =
  "Test RPC functions work when responses are received out of order",
  `Quick,
  fun () ->
    let t =
      let transport = TestPacket.create () in
      T.create transport >>= fun client ->
      let open TestPacket in
      let response1 = Bytes.create 2 in
      let response2 = Bytes.create 2 in
      let t1 = T.rpc client p1 TestPacket.send_request_body (TestPacket.recv_response_body response1) (Ok ()) in
      let t2 = T.rpc client p2 TestPacket.send_request_body (TestPacket.recv_response_body response2) (Ok ()) in
      TestPacket.queue_response r2 transport >>= fun () ->
      TestPacket.queue_response r1 transport >>= fun () ->
      t1 >>|= fun _ -> t2 >>|= fun _ ->
      Lwt.return (response1 = r1.res_payload && response2 = r2.res_payload)
    in Alcotest.(check bool) "Both responses correct" true (Lwt_main.run t)

let test_memory_leak =
  "Check the mux does not have a memory leak",
  `Quick,
  fun () ->
    let t =
      let transport = TestPacket.create () in
      T.create transport >>= fun client ->
      let response1 = Bytes.create 2 in
      TestPacket.record_sequence := false;
      let rec megaqueue n =
        if n=100000 then Lwt.return true
        else
          let t1 = T.rpc client p1 TestPacket.send_request_body (TestPacket.recv_response_body response1) (Ok ()) in
          TestPacket.queue_response r1 transport >>= fun () ->
          t1 >>= fun _ ->
          let ok = if n mod 10000 = 0 then begin
              Gc.compact ();
              let test = Gc.stat () in
              test.Gc.live_words < 100000
            end else true
          in
          if ok then megaqueue (n+1) else Lwt.return false
      in
      megaqueue 0
    in
    Alcotest.(check bool) "Memory leak" true (Lwt_main.run t)

let test_exception_handling =
  "Check that exceptions raised are handled correctly",
  `Quick,
  fun () ->
    let t =
      let transport = TestPacket.create () in
      T.create transport >>= fun client ->
      let open TestPacket in
      let response1 = Bytes.create 2 in
      (* Will return an invalid ID *)
      TestPacket.queue_response { res_id=4; res_payload="exception" |> Bytes.of_string} transport >>= fun () ->
      let t1 = T.rpc client p1 TestPacket.send_request_body (TestPacket.recv_response_body response1) (Ok ()) in
      TestPacket.queue_response r2 transport >>= fun () ->
      Lwt.catch  (fun () -> t1 >>= function Ok _ -> Lwt.return false | Error _ -> Lwt.return true)
        (fun e -> Printf.printf "Exception: %s\n%!" (Printexc.to_string e); Lwt.return true)
    in Alcotest.(check bool) "Exception handled" true (Lwt_main.run t)

let tests =
  "Mux tests",
  [ test_rpc
  ; test_multi_rpc
  ; test_out_of_order_responses
  ; test_memory_leak
  ; test_exception_handling
  ]
