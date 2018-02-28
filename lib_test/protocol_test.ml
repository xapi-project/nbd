(*
 * Copyright (C) 2015 Citrix Systems Inc.
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

(** This module tests the core NBD library by verifying that the communication
    between the client and the server exactly matches the specified test
    sequences. *)

open Nbd
open Lwt.Infix

(** An Alcotest TESTABLE for the data transmissions in the test sequences *)
let transmission =
  let fmt =
    Fmt.of_to_string
      (function
       | `Server, x -> "Server: " ^ (String.escaped x)
       | `Client, x -> "Client: " ^ (String.escaped x))
  in
  Alcotest.testable fmt (=)

(* All the flags in the NBD protocol are in network byte order (big-endian) *)

let option_reply_magic_number = "\x00\x03\xe8\x89\x04\x55\x65\xa9"
let nbd_request_magic = "\x25\x60\x95\x13"
let nbd_reply_magic = "\x67\x44\x66\x98"

(** The client or server wanted to read and there is no more data from the
    other side. *)
exception Failed_to_read_empty_stream

(** [make_channel role test_sequence] creates a channel for use by the NBD library
    from a test sequence containing the expected communication between the
    client and the server. Reads and writes will verify that the communication
    matches exactly what is in [test_sequence], which is a list of data
    transmission tuples, each specifying whether the client or the server is
    sending the data, and the actual data sent. [role] specifies whether the
    client or the server will use the created channel, the other side will be
    simulated by taking the responses from [test_sequence]. *)
let make_channel role test_sequence =
  let next = ref test_sequence in
  let rec read buf =
    (* Ignore reads and writes of length 0 and treat them as a no-op *)
    if Cstruct.len buf = 0 then Lwt.return_unit else
      match !next with
      | (source, x) :: rest ->
        if source = role then failwith "Tried to read but should have written";
        let available = min (Cstruct.len buf) (String.length x) in
        Cstruct.blit_from_string x 0 buf 0 available;
        next := if available = String.length x then rest else (source, (String.sub x available (String.length x - available))) :: rest;
        let buf = Cstruct.shift buf available in
        if Cstruct.len buf = 0
        then Lwt.return ()
        else read buf
      | [] -> Lwt.fail Failed_to_read_empty_stream in
  let rec write buf =
    (* Ignore reads and writes of length 0 and treat them as a no-op *)
    if Cstruct.len buf = 0 then Lwt.return_unit else
      match !next with
      | (source, x) :: rest ->
        if source <> role then failwith "Tried to write but should have read";
        let available = min (Cstruct.len buf) (String.length x) in
        let written = String.sub (Cstruct.to_string buf) 0 available in
        let expected = String.sub x 0 available in
        Alcotest.(check string) "Wrote expected data" expected written;
        next := if available = String.length x then rest else (source, (String.sub x available (String.length x - available))) :: rest;
        let buf = Cstruct.shift buf available in
        if Cstruct.len buf = 0
        then Lwt.return ()
        else write buf
      | [] -> Lwt.fail_with "Tried to write but the stream was empty" in
  let close () = Lwt.return () in
  let assert_processed_complete_sequence () = Alcotest.(check (list transmission)) "processed complete sequence" [] !next in
  (assert_processed_complete_sequence, (read, write, close))

(** Passes a channel for use by the NBD client to the given function, verifying
    that all communcation matches the given test sequence and that the complete
    sequence has been processed after the function returns. *)
let with_client_channel s f =
  fun () ->
    let (assert_processed_complete_sequence, (read, write, close)) = make_channel `Client s in
    f Channel.{read; write; close; is_tls=false};
    assert_processed_complete_sequence ()

(** Passes a channel for use by the NBD server to the given function, verifying
    that all communcation matches the given test sequence and that the complete
    sequence has been processed after the function returns. *)
let with_server_channel s f =
  fun () ->
    let (assert_processed_complete_sequence, (read, write, close)) = make_channel `Server s in
    f Channel.{read_clear=read; write_clear=write; close_clear=close; make_tls_channel=None};
    assert_processed_complete_sequence ()

module V2_negotiation = struct

  let v2_negotiation_start = [
    `Server, "NBDMAGIC";
    `Server, "IHAVEOPT";
    `Server, "\000\001"; (* handshake flags: NBD_FLAG_FIXED_NEWSTYLE *)
    `Client, "\000\000\000\001"; (* client flags: NBD_FLAG_C_FIXED_NEWSTYLE *)

    `Client, "IHAVEOPT";
    `Client, "\000\000\000\001"; (* NBD_OPT_EXPORT_NAME *)
    `Client, "\000\000\000\007"; (* length of export name *)
    `Client, "export1";
  ]

  let v2_negotiation = v2_negotiation_start @ [
    `Server, "\000\000\000\000\001\000\000\000"; (* size *)
    `Server, "\000\001"; (* transmission flags: NBD_FLAG_HAS_FLAGS (bit 0) *)
    `Server, (String.make 124 '\000');
  ]

  let client_negotiation =
    "Perform a negotiation using the second version of the protocol from the
     client's side.",
    `Quick,
    with_client_channel v2_negotiation (fun channel ->
        let t =
          Client.negotiate channel "export1"
          >>= fun (t, size, flags) ->
          Lwt.return ()
        in
        Lwt_main.run t
      )

  let server_negotiation =
    "Perform a negotiation using the second version of the protocol from the
     server's side.",
    `Quick,
    with_server_channel v2_negotiation_start (fun channel ->
        let t =
          Server.connect channel ()
          >>= fun (export_name, svr) ->
          Alcotest.(check string) "The server did not receive the correct export name" "export1" export_name;
          Lwt.return_unit
        in
        Lwt_main.run t
      )
end

module V2_list_export_disabled = struct

  let v2_list_export_disabled = [
    `Server, "NBDMAGIC"; (* read *)
    `Server, "IHAVEOPT";
    `Server, "\000\001"; (* handshake flags: NBD_FLAG_FIXED_NEWSTYLE *)
    `Client, "\000\000\000\001"; (* client flags: NBD_FLAG_C_FIXED_NEWSTYLE *)
    `Client, "IHAVEOPT";
    `Client, "\000\000\000\003"; (* NBD_OPT_LIST *)
    `Client, "\000\000\000\000";
    `Server, option_reply_magic_number;
    `Server, "\000\000\000\003";
    `Server, "\128\000\000\002";
    `Server, "\000\000\000\000";
  ]

  let client_list_disabled =
    "Check that if we request a list of exports and are denied, the error is
     reported properly.",
    `Quick,
    with_client_channel v2_list_export_disabled (fun channel ->
        let t =
          Client.list channel
          >>= function
          | Error `Policy ->
            Lwt.return ()
          | _ -> failwith "Expected to receive a Policy error" in
        Lwt_main.run t
      )

  let server_list_disabled =
    "Check that the server denies listing the exports, and the error is
     reported properly.",
    `Quick,
    with_server_channel v2_list_export_disabled (fun channel ->
        let t () =
          Server.connect channel ()
          >>= fun (_export_name, _svr) ->
          Lwt.return_unit
        in
        (* TODO: The Client.list function currently does not send
           NBD_OPT_ABORT when it should, but incorrectly disconnects, so we
           expect this error from the server side. *)
        try
          Lwt_main.run (t ())
        with Failed_to_read_empty_stream -> ()
      )
end

module V2_list_export_success = struct
  let v2_list_export_success = [
    `Server, "NBDMAGIC"; (* read *)
    `Server, "IHAVEOPT";
    `Server, "\000\001"; (* handshake flags: NBD_FLAG_FIXED_NEWSTYLE *)
    `Client, "\000\000\000\001"; (* client flags: NBD_FLAG_C_FIXED_NEWSTYLE *)
    `Client, "IHAVEOPT";
    `Client, "\000\000\000\003"; (* NBD_OPT_LIST *)
    `Client, "\000\000\000\000";

    `Server, option_reply_magic_number;
    `Server, "\000\000\000\003";
    `Server, "\000\000\000\002"; (* NBD_REP_SERVER *)
    (* TODO: the Client.list function incorrectly parses the server's response:
       it expects one more 32 bit int than what the protocol describes - this
       line shouldn't be here. *)
    `Server, "\000\000\000\011";
    `Server, "\000\000\000\007";
    `Server, "export1";

    `Server, option_reply_magic_number;
    `Server, "\000\000\000\003";
    `Server, "\000\000\000\001"; (* NBD_REP_ACK *)
    `Server, "\000\000\000\000";
  ]

  let client_list_success =
    "Check that if we request a list of exports, a list is returned and parsed
     properly.",
    `Quick,
    with_client_channel v2_list_export_success (fun channel ->
        let t =
          Client.list channel
          >>= function
          | Ok [ "export1" ] ->
            Lwt.return ()
          | _ -> failwith "Expected to receive a list of exports" in
        Lwt_main.run t
      )
end

(** A Mirage block module backed by a Cstruct for unit testing the NBD server *)
module Cstruct_block : (Mirage_block_lwt.S with type t = Cstruct.t) = struct
  type page_aligned_buffer = Cstruct.t
  type error = Mirage_block.error
  let pp_error = Mirage_block.pp_error
  type write_error = Mirage_block.write_error
  let pp_write_error = Mirage_block.pp_write_error
  type 'a io = 'a Lwt.t
  type t = Cstruct.t
  let disconnect _ = Lwt.return_unit
  let get_info contents = Lwt.return Mirage_block.{ read_write = true; sector_size = 1; size_sectors = (Cstruct.len contents |> Int64.of_int) }
  let read contents sector_start buffers =
    let sector_start = Int64.to_int sector_start in
    List.fold_left
      (fun contents buffer -> Cstruct.fillv [contents] buffer |> ignore; Cstruct.shift contents (Cstruct.len buffer))
      (Cstruct.shift contents sector_start)
      buffers
    |> ignore; Lwt.return_ok ()
  let write contents sector_start buffers =
    let sector_start = Int64.to_int sector_start in
    Cstruct.fillv buffers (Cstruct.shift contents sector_start)
    |> ignore; Lwt.return_ok ()
end

module V2_read_only_test = struct

  let test_block = (Cstruct.of_string "asdf")

  let sequence = [
    `Server, "NBDMAGIC";
    `Server, "IHAVEOPT";
    `Server, "\000\001"; (* handshake flags: NBD_FLAG_FIXED_NEWSTYLE *)
    `Client, "\000\000\000\001"; (* client flags: NBD_FLAG_C_FIXED_NEWSTYLE *)

    `Client, "IHAVEOPT";
    `Client, "\000\000\000\001"; (* NBD_OPT_EXPORT_NAME *)
    `Client, "\000\000\000\007"; (* length of export name *)
    `Client, "export1";

    `Server, "\000\000\000\000\000\000\000\004"; (* size: 4 bytes *)
    `Server, "\000\003"; (* transmission flags: NBD_FLAG_READ_ONLY (bit 1) + NBD_FLAG_HAS_FLAGS (bit 0) *)
    `Server, (String.make 124 '\000');
    (* Now we've entered transmission mode *)

    `Client, nbd_request_magic;
    `Client, "\000\000"; (* command flags *)
    `Client, "\000\000"; (* request type: NBD_CMD_READ *)
    `Client, "\000\000\000\000\000\000\000\000"; (* handle: 4 bytes *)
    `Client, "\000\000\000\000\000\000\000\001"; (* offset *)
    `Client, "\000\000\000\002"; (* length *)

    (* We're allowed to read from a read-only export *)
    `Server, nbd_reply_magic;
    `Server, "\000\000\000\000"; (* error: no error *)
    `Server, "\000\000\000\000\000\000\000\000"; (* handle *)
    `Server, "sd"; (* 2 bytes of data *)

    `Client, nbd_request_magic;
    `Client, "\000\000"; (* command flags *)
    `Client, "\000\001"; (* request type: NBD_CMD_WRITE *)
    `Client, "\000\000\000\000\000\000\000\001"; (* handle: 4 bytes *)
    `Client, "\000\000\000\000\000\000\000\000"; (* offset *)
    `Client, "\000\000\000\004"; (* length *)
    (* The server should probably return the EPERM error immediately, and not
       read any data associated with the write request, as the client should
       recognize the error before transmitting the data, just like for EINVAL,
       which is sent for unaligned requests. *)
    (*`Client, "nope"; (* 4 bytes of data *)*)

    (* However, we're not allowed to write to it *)
    `Server, nbd_reply_magic;
    `Server, "\000\000\000\001"; (* error: EPERM *)
    `Server, "\000\000\000\000\000\000\000\001"; (* handle *)

    (* TODO: currently the test fails with the below lines uncommented, because
       the server disconnects in case of write errors, but according to the NBD
       protocol it probably shouldn't, it should continue to process the
       client's requests *)
    (*
    `Client, nbd_request_magic;
    `Client, "\000\000"; (* command flags *)
    `Client, "\000\002"; (* request type: NBD_CMD_DISC *)
    `Client, "\000\000\000\000\000\000\000\002"; (* handle: 4 bytes *)
    `Client, "\000\000\000\000\000\000\000\000"; (* offset *)
    `Client, "\000\000\000\000"; (* length *)
    *)
  ]

  let server_test =
    "Serve a read-only export and test that reads and writes are handled correctly.",
    `Quick,
    with_server_channel sequence (fun channel ->
        let t =
          Server.connect channel ()
          >>= fun (export_name, svr) ->
          Alcotest.(check string) "The server did not receive the correct export name" "export1" export_name;
          Server.serve svr ~read_only:true (module Cstruct_block) test_block
        in
        Lwt_main.run t
      )

end

let tests =
  "Nbd client tests",
  [ V2_negotiation.client_negotiation
  ; V2_negotiation.server_negotiation
  ; V2_list_export_disabled.client_list_disabled
  ; V2_list_export_disabled.server_list_disabled
  ; V2_list_export_success.client_list_success
  ; V2_read_only_test.server_test
  ]
