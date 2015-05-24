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

type t
(** An open connection to an NBD client *)

type size = int64
(** The size of a remote disk *)

val negotiate : channel -> size  -> Nbd.PerExportFlag.t list -> t Lwt.t
(** [negotiate channel size flags] tells the client connected to [channel] about the
    disk [size] and [flags]. When negotiate finishes, the client will start
    sending requests. *)

val close: t -> unit Lwt.t
(** [close t] shuts down the connection [t] and frees any allocated resources *)

val next : t -> Nbd.Request.t Lwt.t
(** [next t] returns the next RBD request *)

val ok : t -> int64 -> Cstruct.t option -> unit Lwt.t
(** [ok t handle data] replies affirmatively to the request identified
    by [handle] with optional response payload [data] *)

val error : t -> int64 -> Nbd.Error.t -> unit Lwt.t
(** [error t handle code] sends error [code] to the client [t[ to
    indicate that the request failed. *)

