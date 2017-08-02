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

(** Network Block Device client and servers for Unix *)

open Nbd

type tls_role =
  | TlsClient of Ssl.context option
  | TlsServer of Ssl.context option

val connect: string -> int -> Channel.channel Lwt.t
(** [connect hostname port] connects to host:port and returns
    a channel. *)

val cleartext_channel_of_fd: Lwt_unix.file_descr -> tls_role -> Channel.cleartext_channel
(** [cleartext_channel_of_fd fd] returns a channel from an existing file descriptor *)

val init_tls_get_ctx: certfile:string -> ciphersuites:string -> Ssl.context
(** Initialise the Ssl (TLS) library and then create and return a new context *)

module Client: S.CLIENT
(** A client allows you to access remote disks *)

module Server: S.SERVER
(** A server allows you to expose disks to remote clients *)
