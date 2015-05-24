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

type channel = {
  read:  Cstruct.t -> unit Lwt.t;
  write: Cstruct.t -> unit Lwt.t;
  close: unit -> unit Lwt.t;
}

val connect: string -> int -> channel Lwt.t
(** [connect hostname port] connects to host:port and returns
    a channel. *)

val of_fd: Lwt_unix.file_descr -> channel
