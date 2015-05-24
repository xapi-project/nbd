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
open Nbd_lwt_channel

type name = string

type 'a t = {
  channel: channel;
  request: Cstruct.t; (* buffer used to read the request headers *)
  reply: Cstruct.t;   (* buffer used to write the response headers *)
  m: Lwt_mutex.t; (* prevents partial message interleaving *)
}

type size = int64

let close t = t.channel.close ()

let negotiate channel size flags =
  let buf = Cstruct.create Announcement.sizeof in
  Announcement.(marshal buf `V1);
  channel.write buf
  >>= fun () ->
  let buf = Cstruct.create (Negotiate.sizeof `V1) in
  Negotiate.(marshal buf (V1 { size; flags }));
  channel.write buf
  >>= fun () ->
  let request = Cstruct.create Request.sizeof in
  let reply = Cstruct.create Reply.sizeof in
  let m = Lwt_mutex.create () in
  return { channel; request; reply; m }

let negotiate_begin channel ?offer () = fail (Failure "unimplemented")
let negotiate_end t size flags = fail (Failure "unimplemented")

let next t =
  t.channel.read t.request
  >>= fun () ->
  match Request.unmarshal t.request with
  | `Ok r -> return r
  | `Error e -> fail e

let ok t handle payload =
  Lwt_mutex.with_lock t.m
    (fun () ->
      Reply.marshal t.reply { Reply.handle; error = `Ok () };
      t.channel.write t.reply
      >>= fun () ->
      match payload with
      | None -> return ()
      | Some data -> t.channel.write data
    )

let error t handle code =
  Lwt_mutex.with_lock t.m
    (fun () ->
      Reply.marshal t.reply { Reply.handle; error = `Error code };
      t.channel.write t.reply
    )

