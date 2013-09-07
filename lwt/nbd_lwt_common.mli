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

val really_read: Lwt_unix.file_descr -> Lwt_bytes.t -> int -> unit Lwt.t
(** [really_read fd buffer num] reads exactly [num] bytes into [buffer]
    from [fd] or fails *)

val really_write: Lwt_unix.file_descr -> Lwt_bytes.t -> int -> unit Lwt.t
(** [really_write fd buffer num] writes exactly [num] bytes from [buffer]
    to [fd] or fails *)
