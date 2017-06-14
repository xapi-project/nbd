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

(** Channels represent connections between clients and servers. *)

type basic_channel = {
  read:  Cstruct.t -> unit Lwt.t; (** Read a block of data from the channel *)
  write: Cstruct.t -> unit Lwt.t; (** Write a block of data to the channel *)
  close: unit -> unit Lwt.t; (** Close the channel *)
}
(** An open channel to an NBD client or server. *)

type tls_channel = {
  read_tls: Cstruct.t -> unit Lwt.t;
  write_tls: Cstruct.t -> unit Lwt.t;
  close_tls: unit -> unit Lwt.t;
}

type tls_role = Client | Server

type cleartext_channel =  {
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

val generic_of_tls_channel: tls_channel Lwt.t -> generic_channel Lwt.t

val generic_of_cleartext_channel: cleartext_channel Lwt.t -> generic_channel Lwt.t
