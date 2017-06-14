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

type basic_channel = {
  read: Cstruct.t -> unit Lwt.t;
  write: Cstruct.t -> unit Lwt.t;
  close: unit -> unit Lwt.t;
}

type tls_channel = {
  read_tls: Cstruct.t -> unit Lwt.t;
  write_tls: Cstruct.t -> unit Lwt.t;
  close_tls: unit -> unit Lwt.t;
}

type tls_role = Client | Server

type cleartext_channel = {
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

let generic_of_tls_channel tch = tch >>= fun ch -> return {
  read = ch.read_tls;
  write = ch.write_tls;
  close = ch.close_tls;
  is_tls = true;
  make_tls = None;
}

let generic_of_cleartext_channel cch = cch >>= fun ch -> return
  {
    read = ch.read_clear;
    write = ch.write_clear;
    close = ch.close_clear;
    is_tls = false;
    make_tls = Some (ch.make_tls_channel);
  }
