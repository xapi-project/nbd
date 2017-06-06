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

let return = Lwt.return
let (>>=) = Lwt.(>>=)

type tls_role =
  | TlsClient of Ssl.context option
  | TlsServer of Ssl.context option

let debug_io = ref false

(* TODO move this function into a library used by this and other executables *)
(* Also the read/write/close functions that are inside tls_channel_of_fd *)
let io_complete name offset op fd buffer =
  if !debug_io
  then Printf.fprintf stderr "%s offset=%s length=%d\n%!" name (match offset with Some x -> Int64.to_string x | None -> "None") (Cstruct.len buffer);
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
  then Lwt.fail End_of_file
  else return ()

let tls_channel_of_fd fd role ctx () =
  let ctx, ssl_start =
    match role with
      | TlsClient _ -> ctx, Lwt_ssl.ssl_connect
      | TlsServer _ -> ctx, Lwt_ssl.ssl_accept
  in
  ssl_start fd ctx >>= fun sock ->
  let offset = ref 0L in

  let read_tls buf =
    io_complete "read" (Some !offset) Lwt_ssl.read_bytes sock buf >>= fun () ->
    offset := Int64.(add !offset (of_int (Cstruct.len buf)));
    return () in

  let write_tls buf =
    io_complete "write" (Some !offset) Lwt_ssl.write_bytes sock buf >>= fun () ->
    offset := Int64.(add !offset (of_int (Cstruct.len buf)));
    return () in

  let close_tls () =
    ignore (Lwt_ssl.ssl_shutdown sock);
    Lwt_ssl.close sock in

  return { read_tls; write_tls; close_tls }


let cleartext_channel_of_fd fd role =
  let read_clear = Lwt_cstruct.(complete (read fd)) in
  let write_clear = Lwt_cstruct.(complete (write fd)) in
  let close_clear () = Lwt_unix.close fd in
  let make_tls_channel = match role with
    | TlsClient None
    | TlsServer None ->
        raise (Failure "Internal error: confusion over whether to use TLS.")
    | TlsClient (Some ctx)
    | TlsServer (Some ctx) -> tls_channel_of_fd fd role ctx
  in
  { read_clear; write_clear; close_clear; make_tls_channel }

let generic_channel_of_fd fd role =
  let ch = cleartext_channel_of_fd fd role
  in return (Channel.generic_of_cleartext_channel ch)

let connect hostname port =
  let socket = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.gethostbyname hostname
  >>= fun host_info ->
  let server_address = host_info.Lwt_unix.h_addr_list.(0) in
  Lwt_unix.connect socket (Lwt_unix.ADDR_INET (server_address, port))
  >>= fun () ->
  (generic_channel_of_fd socket (TlsClient None))

let init_tls_get_ctx ~certfile ~ciphersuites =
  Ssl_threads.init ();
  Ssl.init ();
  let mk_ctx role_ctx = Ssl.create_context Ssl.TLSv1_2 role_ctx in
  let ctx = mk_ctx Ssl.Server_context in
  Ssl.use_certificate ctx certfile certfile; (* Second one is being used as privkey filename *)
  Ssl.set_cipher_list ctx ciphersuites;
  ctx

module Client = Nbd.Client
module Server = Nbd.Server
