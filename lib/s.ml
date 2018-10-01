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
open Channel
open Result

(** Common signatures used in the library. *)

type write_error = [ Mirage_block.write_error | `Protocol_error of Protocol.Error.t ]
type error = [ Mirage_block.error | `Protocol_error of Protocol.Error.t ]

module type CLIENT = sig
  (** A Client allows you to list the disks available on a server, connect to
      a specific disk and then issue read and write requests. *)

  include Mirage_block_lwt.S
    with type error = error
     and type write_error = write_error

  type size = int64
  (** The size of a remote disk *)

  val list: channel -> (string list, [ `Policy | `Unsupported ]) result Lwt.t
  (** [list channel] returns a list of exports known by the server.
      [`Error `Policy] means the server has this function disabled deliberately.
      [`Error `Unsupported] means the server is old and does not support the query
      function. *)

  val negotiate: channel -> string -> (t * size * Protocol.PerExportFlag.t list) Lwt.t
  (** [negotiate channel export] takes an already-connected channel,
      performs the initial protocol negotiation and connects to
      the named export. Returns [disk * remote disk size * flags] *)

  (** {2 Option negotiation} *)

  (** {!negotiate} above is a convenience function that immediately connects to
      the specified export, and enters the transmission phase. However, the
      fixed-newstyle version of the NBD protocol allows one to negotiate options
      with the server before entering transmission phase. The below functions
      allow one to interact with a server supporting fixed-newstyle negotiation
      and then select the desired export. *)

  type client
  (** Represents a client connected to the server that is still in the option
      haggling phase *)

  val connect: channel -> client Lwt.t
  (** Takes a channel, and performs the initial handshake without connecting to
      an export. Fails if the server does not support fixed-newstyle negotiation. *)

  val negotiate_structured_reply: client -> (unit, Protocol.OptionError.t) result Lwt.t
  (** Enable structured replies during the transmission phase. *)

  val list_meta_contexts: client -> string -> string list -> (string list, Protocol.OptionError.t) result Lwt.t
  (** [list_meta_contexts client export queries] returns the metadata contexts
      available to the client that match one of the given queries.
      Structured replies must be negotiated first. *)

  val set_meta_contexts: client -> string -> string list -> ((int32 * string) list, Protocol.OptionError.t) result Lwt.t
  (** [set_meta_contexts client export queries] changes the set of active
      metadata contexts for the specified export. Structured replies must be
      negotiated first.
      Returns the list of selected metadata contexts, each with a unique meta
      context ID. *)

  val query_info: client -> string -> Protocol.Info.t list -> (Protocol.InfoResponse.t list, Protocol.OptionError.t) result Lwt.t

  val request_export: client -> string -> ((t * Protocol.DiskInfo.t * Protocol.InfoResponse.block_size option), Protocol.OptionError.t) result Lwt.t
  (** Connect to the specified export and enter tansmission phase.
      If the returned block size information is not None, the client MUST
      respect these limits. *)

  val abort: client -> unit Lwt.t
  (** Abort the negotiation and terminate the session without connecting to an
      export *)

  (** {2 Transmission phase} *)

  val query_block_status: t -> int64 -> int32 -> (Protocol.StructuredReplyChunk.BlockStatusChunk.t list, error) result Lwt.t
  (** [query_block_status client offset length] returns a list of block status
      descriptors for the specified area, for each of the metadata contexts
      selected by [set_meta_contexts].

      The returned descriptors describe non-overlapping, consecutive portions
      of the export, starting at the specified [offset]. Thus the offset of a
      given descriptor can be obtained by summing the lengths of all the
      descriptors before it.

      The server will return at least one descriptor, and the length of every
      descriptor will be an integer multiple of the minimum block size, if it
      has been advertised by the server.

      The server may return less data than requested: the sum of the
      descriptors' lengths might be smaller than the specified [length].
      On the other hand, the server may also return more data than requested:
      the final descriptor may extend beyond the original requested size - the
      sum of the [length] fields of the first N-1 desriptors will always be
      less than the requested [length], while the length of the final extent
      may result in a larger sum.

      In case the [base:allocation] metadata context has been selected, the
      {!Protocol.AllocationMetadataFlag.of_int32} function can be used to
      decode the status flags. *)

  val read_chunked : t -> int64 -> int32
    -> ([`Data of Channel.generic_channel * size * int32 | `Hole of Protocol.StructuredReplyChunk.OffsetHoleChunk.t] -> unit Lwt.t)
    -> (unit, Protocol.Error.t) result Lwt.t

  val write_zeroes : t -> int64 -> int32 -> (unit, write_error) result Lwt.t
end

module type SERVER = sig
  (** A Server allows you to expose an existing block device to remote clients
      over NBD. *)

  type t
  (** An open connection to an NBD client *)

  type size = int64
  (** The size of a remote disk *)

  type name = string
  (** The name of an export. In the 'new style' protocol as used in nbd >= 2.9.17
      the client must select an export by name. *)

  exception Client_requested_abort
  (** The client terminated the option haggling phase by sending NBD_OPT_ABORT *)

  val connect : cleartext_channel -> ?offer:name list -> unit -> (name * t) Lwt.t
  (** [connect cleartext_channel ?offer ()] performs the 'new style' initial
      handshake and options negotiation.
      Note that FORCEDTLS mode will be used in the negotiation unless
      [cleartext_channel.make_tls_channel] is None, signifying NOTLS mode.
      If [?offer] is provided then these names will be returned if the client
      requests a list of exports, otherwise we will return EPERM.
      The client's choice of name is returned which must be looked up by the
      application. If the name is invalid, the only option is to close the connection.
      If the name is valid then use the [serve] function.

      Raises {!Client_requested_abort} if the client aborts the option haggilng
      phase instead of entering the transmission phase *)

  val serve : t -> ?read_only:bool -> (module Mirage_block_lwt.S with type t = 'b) -> 'b -> unit Lwt.t
  (** [serve t read_only block b] runs forever processing requests from [t], using [block]
      device type [b]. If [read_only] is true, which is the default, the
      [block] device [b] is served in read-only mode: the server will set the
      NBD_FLAG_READ_ONLY transmission flag, and if the client issues a write
      command, the server will send an EPERM error to the client and will
      terminate the session. *)

  val proxy : t -> ?read_only:bool -> (module CLIENT with type t = 'b) -> 'b -> unit Lwt.t

  val close: t -> unit Lwt.t
  (** [close t] shuts down the connection [t] and frees any allocated resources *)

  val with_connection :
    Channel.cleartext_channel ->
    ?offer:name list ->
    (string -> t -> unit Lwt.t) ->
    unit Lwt.t
    (** [with_connection clearchan ~offer f] calls [connect clearchan ~offer] and
        attempts to apply [f] to the resulting [t], with a guarantee to call
        [close t] afterwards. *)

end
