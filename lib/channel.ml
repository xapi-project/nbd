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

type basic_channel = {
  read: Cstruct.t -> unit Lwt.t;
  write: Cstruct.t -> unit Lwt.t;
  close: unit -> unit Lwt.t;
}

type tls_channel = {
  read_tls: Cstruct.t -> unit Lwt.t;
  write_tls: Cstruct.t -> unit Lwt.t;
  close_tls: unit -> unit Lwt.t;
}

type tls_role = Client | Server

type cleartext_channel = {
  read_clear: Cstruct.t -> unit Lwt.t;
  write_clear: Cstruct.t -> unit Lwt.t;
  close_clear: unit -> unit Lwt.t;

  make_tls_channel: unit -> tls_channel Lwt.t;
}

type generic_channel = {
  is_tls: bool;
  make_tls: (unit -> tls_channel Lwt.t) option;
  read: Cstruct.t -> unit Lwt.t;
  write: Cstruct.t -> unit Lwt.t;
  close: unit -> unit Lwt.t;
}

type channel = generic_channel

type polymorphic_channel =
  | Tls_channel of tls_channel
  | Cleartext_channel of cleartext_channel

let generic_of_tls_channel tch = tch >>= fun ch -> return {
  read = ch.read_tls;
  write = ch.write_tls;
  close = ch.close_tls;
  is_tls = true;
  make_tls = None;
}

let generic_of_cleartext_channel cch = cch >>= fun ch -> return
  {
    read = ch.read_clear;
    write = ch.write_clear;
    close = ch.close_clear;
    is_tls = false;
    make_tls = Some (ch.make_tls_channel);
  }

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

let tls_of_fd fd role () =
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
