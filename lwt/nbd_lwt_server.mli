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

type 'a t
(** An open connection to an NBD client *)

type size = int64
(** The size of a remote disk *)

val negotiate : channel -> size  -> Nbd.PerExportFlag.t list -> [ `Connected ] t Lwt.t
(** [negotiate channel size flags] tells the client connected to [channel] about the
    disk [size] and [flags]. When negotiate finishes, the client will start
    sending requests. This is the 'old style' protocol as used in nbd <= 2.9.16.
  *)

type name = string
(** The name of an export. In the 'new style' protocol as used in nbd >= 2.9.17
    the client must select an export by name. *)

val negotiate_begin : channel -> ?offer:name list -> unit -> (name * [ `Pending ] t) Lwt.t
(** [negotiate_begin channel ?offer ()] performs the 'new style' initial handshake
    and options negotiation. If ?offer is provided then these names will be returned
    if the client requests a list of exports, otherwise we will return EPERM.
    The client's choice of name is returned which must be confirmed or denied before
    the connection is `Connected. *)

val negotiate_end : [ `Pending ] t -> size -> Nbd.PerExportFlag.t list -> [ `Connected ] t Lwt.t
(** [negotiate_end t size flags] completes the 'new style' initial handshake by
    returning the [size] and [flags] to the client. The connection can now be used
    for I/O. *)

val serve_forever : [ `Pending ] t ->  (module V1_LWT.BLOCK with type t = 'b) -> 'b -> unit Lwt.t
(** [serve_forever t block b] runs forever processing requests from [t], using [block]
    device type [b]. *)

val close: 'a t -> unit Lwt.t
(** [close t] shuts down the connection [t] and frees any allocated resources *)

val next : [ `Connected ] t -> Nbd.Request.t Lwt.t
(** [next t] returns the next RBD request *)

val ok : [ `Connected ] t -> int64 -> Cstruct.t option -> unit Lwt.t
(** [ok t handle data] replies affirmatively to the request identified
    by [handle] with optional response payload [data] *)

val error : [ `Connected ] t -> int64 -> Nbd.Error.t -> unit Lwt.t
(** [error t handle code] sends error [code] to the client [t[ to
    indicate that the request failed. *)

