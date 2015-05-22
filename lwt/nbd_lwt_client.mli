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

type t
(** An open connection to an NBD server *)

type size = int64
(** The size of a remote disk *)

type channel = {
  read:  Cstruct.t -> unit Lwt.t;
  write: Cstruct.t -> unit Lwt.t;
}

val open_channel: string -> int -> channel Lwt.t
(** [open_channel hostname port] connects to host:port and returns
    a channel. *)

val negotiate: channel -> string -> (t * size * Nbd.Flag.t list) Lwt.t
(** [negotiate channel export] takes an already-connected channel,
    performs the initial protocol negotiation and connects to
    the named export. Returns (remote disk size * flags) *)

val write : t -> Cstruct.t -> int64 -> unit Lwt.t
(** [write t buf dst_offset] writes the whole string [buf] to
    [dst_offset] in the remote disk. *)

val read : t -> int64 -> int32 -> Cstruct.t Lwt.t
(** [read t offset len] reads [len] bytes from the remote disk starting
    at [offset] *)
