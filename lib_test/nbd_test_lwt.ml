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

let make len char =
  let buf = Cstruct.create len in
  for i = 0 to len - 1 do
    Cstruct.set_char buf i char
  done;
  buf

open Nbd_lwt_client

let test host port =
  try_lwt
    let test_str1 = make 512 'a' in
    let test_str2 = make 512 'b' in
    let test_str3 = make 512 'c' in
    let test_str4 = make 512 'd' in
    let test_str5 = make 512 'e' in
    let test_str6 = make 512 'f' in
    let test_str7 = make 512 'g' in
    let test_str8 = make 512 'h' in
    Printf.printf "Connecting...\n";
    lwt channel = open_channel host port in
    lwt (sock,sz,flags) = negotiate channel "export" in
    Printf.printf "Connected: size=%Ld\n" sz;
    let t1 = write sock test_str1 0L in
    let t2 = write sock test_str2 512L in
    let t3 = write sock test_str3 1024L in
    let t4 = write sock test_str4 1536L in
    let t5 = write sock test_str5 2048L in
    let t6 = write sock test_str6 2560L in
    let t7 = write sock test_str7 3072L in
    let t8 = write sock test_str8 3584L in

    lwt () = Lwt.join [t1; t2; t3; t4; t5; t6; t7; t8] in
    Printf.printf "Written\n";
    read sock 0L 4096l
    >>= function
    | `Ok str2 ->
      Printf.printf "%s\n" (Cstruct.to_string str2);
      Lwt.return ()
    | `Error e -> failwith (Printf.sprintf "Read failed with %s" (Nbd.Error.to_string e))
  with e -> 
    Printf.printf "Caught exception: %s" (Printexc.to_string e);
    Lwt.return ()

let _ =
  Lwt_main.run (test Sys.argv.(1) (int_of_string Sys.argv.(2)))

