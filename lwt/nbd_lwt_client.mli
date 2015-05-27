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
open Nbd_lwt_channel

include V1_LWT.BLOCK

type size = int64
(** The size of a remote disk *)

val list: channel -> [ `Ok of string list | `Error of [ `Policy | `Unsupported ] ] Lwt.t
(** [list channel] returns a list of exports known by the server.
    `Error `Policy means the server has this function disabled deliberately.
    `Error `Unsupported means the server is old and does not support the query
    function. *)

val negotiate: channel -> string -> (t * size * Nbd.PerExportFlag.t list) Lwt.t
(** [negotiate channel export] takes an already-connected channel,
    performs the initial protocol negotiation and connects to
    the named export. Returns (remote disk size * flags) *)
