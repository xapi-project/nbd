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

val connect : string -> int -> (t * size * Nbd.flag list) Lwt.t
(** [connect hostname port] connects to an NBD server and performs
    the initial protocol negotiation. Returns
    (connected Unix.file_descr * remote disk size * flags) *)

val negotiate : Lwt_unix.file_descr -> (t * size * Nbd.flag list) Lwt.t
(** [negotiate fd] takes an already-connected Unix.file_descr and
    performs the initial protocol negotiation. Returns
    (remote disk size * flags) *)

val write : t -> string -> int64 -> unit Lwt.t
(** [write t buf dst_offset] writes the whole string [buf] to
    [dst_offset] in the remote disk. *)

val read : t -> int64 -> int32 -> string Lwt.t
(** [read t offset len] reads [len] bytes from the remote disk starting
    at [offset] *)
