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
open Nbd
open Channel
open Lwt

let ciphersuites = "!EXPORT:RSA+AES128-SHA256"

let _ = (
  Ssl_threads.init ();
  Ssl.init ()
)

let debug_io = ref false

let io_complete name offset op fd buffer =
  if !debug_io
  then Printf.fprintf stderr "%s offset=%s length=%d\n%!" name (match offset with Some x -> Int64.to_string x | None -> "None") (Cstruct.len buffer);
  let open Lwt in
  let ofs = buffer.Cstruct.off in
  let len = buffer.Cstruct.len in
  let buf = buffer.Cstruct.buffer in
  let rec loop acc fd buf ofs len =
    op fd buf ofs len >>= fun n ->
    let len' = len - n in
    let acc' = acc + n in
    if len' = 0 || n = 0
    then return acc'
    else loop acc' fd buf (ofs + n) len' in
  loop 0 fd buf ofs len >>= fun n ->
  if n = 0 && len <> 0
  then fail End_of_file
  else return ()

let tls_channel_of_fd fd role () =
  let role_ctx, ssl_start = match role with
    | Client -> Ssl.Client_context, Lwt_ssl.ssl_connect
    | Server -> Ssl.Server_context, Lwt_ssl.ssl_accept in

  let ctx = Ssl.create_context Ssl.TLSv1_2 role_ctx in
  Ssl.set_cipher_list ctx ciphersuites;

  ssl_start fd ctx >>= fun sock ->
  let offset = ref 0L in

  let really_read buf =
    io_complete "read" (Some !offset) Lwt_ssl.read_bytes sock buf >>= fun () ->
    offset := Int64.(add !offset (of_int (Cstruct.len buf)));
    return () in

  let really_write buf =
    io_complete "write" (Some !offset) Lwt_ssl.write_bytes sock buf >>= fun () ->
    offset := Int64.(add !offset (of_int (Cstruct.len buf)));
    return () in

  let close () =
    ignore (Lwt_ssl.ssl_shutdown sock);
    Lwt_ssl.close sock in

  return { read_tls=really_read; write_tls=really_write; close_tls=close }


let of_fd fd role =
  let read = Lwt_cstruct.(complete (read fd)) in
  let write = Lwt_cstruct.(complete (write fd)) in
  let close () = Lwt_unix.close fd in
  let make_tls = Some (fun () ->
    tls_channel_of_fd fd role ()
  ) in
  { read; write; close; is_tls=false; make_tls}

let connect hostname port =
  let socket = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.gethostbyname hostname
  >>= fun host_info ->
  let server_address = host_info.Lwt_unix.h_addr_list.(0) in
  Lwt_unix.connect socket (Lwt_unix.ADDR_INET (server_address, port))
  >>= fun () ->
  return (of_fd socket Channel.Client)

module Client = Nbd.Client
module Server = Nbd.Server
