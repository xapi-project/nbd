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

open OUnit
open Nbd
open Lwt

(* All the flags in the NBD protocol are in network byte order (big-endian) *)

let v2_negotiation = [
  `Server "NBDMAGIC";
  `Server "IHAVEOPT";
  `Server "\000\001"; (* handshake flags: NBD_FLAG_FIXED_NEWSTYLE *)
  `Client "\000\000\000\001"; (* client flags: NBD_FLAG_C_FIXED_NEWSTYLE *)
  `Client "IHAVEOPT";
  `Client "\000\000\000\001"; (* NBD_OPT_EXPORT_NAME *)
  `Client "\000\000\000\007";
  `Client "export1";
  `Server "\000\000\000\000\001\000\000\000"; (* size *)
  `Server "\000\000"; (* transmission flags *)
  `Server (String.make 124 '\000');
]

let v2_list_export_disabled = [
  `Server "NBDMAGIC"; (* read *)
  `Server "IHAVEOPT";
  `Server "\000\001"; (* handshake flags: NBD_FLAG_FIXED_NEWSTYLE *)
  `Client "\000\000\000\001"; (* client flags: NBD_FLAG_C_FIXED_NEWSTYLE *)
  `Client "IHAVEOPT";
  `Client "\000\000\000\003"; (* NBD_OPT_LIST *)
  `Client "\000\000\000\000";
  `Server "\x00\x03\xe8\x89\x04\x55\x65\xa9";
  `Server "\000\000\000\003";
  `Server "\128\000\000\002";
  `Server "\000\000\000\000";
]

let v2_list_export_success = [
  `Server "NBDMAGIC"; (* read *)
  `Server "IHAVEOPT";
  `Server "\000\001"; (* handshake flags: NBD_FLAG_FIXED_NEWSTYLE *)
  `Client "\000\000\000\001"; (* client flags: NBD_FLAG_C_FIXED_NEWSTYLE *)
  `Client "IHAVEOPT";
  `Client "\000\000\000\003"; (* NBD_OPT_LIST *)
  `Client "\000\000\000\000";

  `Server "\x00\x03\xe8\x89\x04\x55\x65\xa9";
  `Server "\000\000\000\003";
  `Server "\000\000\000\002"; (* NBD_REP_SERVER *)
  `Server "\000\000\000\011";
  `Server "\000\000\000\007";
  `Server "export1";

  `Server "\x00\x03\xe8\x89\x04\x55\x65\xa9";
  `Server "\000\000\000\003";
  `Server "\000\000\000\001"; (* NBD_REP_ACK *)
  `Server "\000\000\000\000";
]

let make_client_channel test_sequence =
  let next = ref test_sequence in
  let rec read buf = match !next with
    | `Server x :: rest ->
      let available = min (Cstruct.len buf) (String.length x) in
      Cstruct.blit_from_string x 0 buf 0 available;
      next := if available = String.length x then rest else `Server (String.sub x available (String.length x - available)) :: rest;
      let buf = Cstruct.shift buf available in
      if Cstruct.len buf = 0
      then return ()
      else read buf
    | `Client _ :: _ -> fail (Failure "Client tried to read but it should have written")
    | [] -> fail (Failure "Client tried to read but the stream was empty") in
  let rec write buf = match !next with
    | `Server _ :: _ -> fail (Failure "Client tried to write but it should have read")
    | `Client x :: rest ->
      let available = min (Cstruct.len buf) (String.length x) in
      let written = String.sub (Cstruct.to_string buf) 0 available in
      let expected = String.sub x 0 available in
      OUnit.assert_equal ~msg:(Printf.sprintf "client wrote bytes: '%s' (length: %d), expected: '%s' (length: %d)" (String.escaped written) (String.length written) (String.escaped expected) (String.length expected)) expected written;
      next := if available = String.length x then rest else `Client (String.sub x available (String.length x - available)) :: rest;
      let buf = Cstruct.shift buf available in
      if Cstruct.len buf = 0
      then return ()
      else write buf
    | [] -> fail (Failure "Client tried to write but the stream was empty") in
  let close () = return () in
  Channel.{ read; write; close; is_tls=false }

let client_negotiation =
  "Perform a negotiation using the second version of the protocol from the
   client's side."
  >:: fun () ->
    let t =
      let channel = make_client_channel v2_negotiation in
      Client.negotiate channel "export1"
      >>= fun (t, size, flags) ->
      return () in
    Lwt_main.run t

let list_disabled =
  "Check that if we request a list of exports and are denied, the error is
   reported properly."
  >:: fun () ->
    let t =
      let channel = make_client_channel v2_list_export_disabled in
      Client.list channel
      >>= function
      | `Error `Policy ->
        return ()
      | _ -> failwith "Expected to receive a Policy error" in
    Lwt_main.run t

let list_success =
  "Check that if we request a list of exports, a list is returned and parsed
   properly."
  >:: fun () ->
    let t =
      let channel = make_client_channel v2_list_export_success in
      Client.list channel
      >>= function
      | `Ok [ "export1" ] ->
        return ()
      | _ -> failwith "Expected to receive a list of exports" in
    Lwt_main.run t

let tests =
  "Nbd client tests" >::: [
    client_negotiation;
    list_disabled;
    list_success;
  ]
