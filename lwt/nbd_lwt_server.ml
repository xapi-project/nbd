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

type t = {
  fd: Lwt_unix.file_descr;
  request: Cstruct.t; (* buffer used to read the request headers *)
  reply: Cstruct.t;   (* buffer used to write the response headers *)
  m: Lwt_mutex.t; (* prevents partial message interleaving *)
}

type size = int64

let close t = Lwt_unix.close t.fd

let negotiate fd size flags =
  let buf = Cstruct.create Announcement.sizeof in
  Announcement.(marshal buf `V1);
  really_write fd buf
  >>= fun () ->
  let buf = Cstruct.create (Negotiate.sizeof `V1) in
  Negotiate.(marshal buf (V1 { size; flags }));
  really_write fd buf
  >>= fun () ->
  let request = Cstruct.create Request.sizeof in
  let reply = Cstruct.create Reply.sizeof in
  let m = Lwt_mutex.create () in
  return { fd; request; reply; m }

let next t =
  lwt () = really_read t.fd t.request in
  match Request.unmarshal t.request with
  | `Ok r -> return r
  | `Error e -> fail e

let ok t handle payload =
  Lwt_mutex.with_lock t.m
    (fun () ->
      Reply.marshal t.reply { Reply.handle; error = 0l };
      lwt () = really_write t.fd t.reply in
      match payload with
      | None -> return ()
      | Some data -> really_write t.fd data
    )

let error t handle code =
  Lwt_mutex.with_lock t.m
    (fun () ->
      Reply.marshal t.reply { Reply.handle; error = code };
      really_write t.fd t.reply
    )

