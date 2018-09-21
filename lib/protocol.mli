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

(** Types representing NBD protocol requests and responses. *)

(** {2 Constants} *)

module Error: sig
  (** Read and write requests can fail with an error response. *)

  type t = [
    | `EPERM  (** Operation not permitted *)
    | `EIO    (** Input/output error *)
    | `ENOMEM (** Cannot allocate memory *)
    | `EINVAL (** Invalid argument *)
    | `ENOSPC (** No space left on device *)
    | `EOVERFLOW (** Value too large *)
    | `ESHUTDOWN (** Server is in the process of being shut down *)
    | `Unknown of int32
  ]
  (** Defined error codes which can be returned in response to a request
      in the data-pushing phase. *)

  val to_string: t -> string
end

module Command: sig
  (** Once a connection has been established, the client can submit commands. *)

  type t =
    | Read  (** Read a block of data *)
    | Write (** Write a block of data *)
    | Disc  (** Disconnect: server must flush all outstanding commands and then
                will close the connection *)
    | Flush (** A flush request or write barrier. All requests received before
                this one will have completed before this command is acknowledged. *)
    | Trim  (** A hint that a data region is nolonger required and may be
                discarded. *)
    | WriteZeroes (** A write request with no payload to zero out data on disk *)
    | BlockStatus (** A block status query request. *)
    | Unknown of int32 (** A command which this protocol implementation doesn't
                           support. *)

  val to_string: t -> string
end

module PerExportFlag: sig
  (** Every disk (or 'export') has a number of associated flags. This will
      be returned from the server when the negotiation is complete. *)

  type t =
    | Read_only   (** export is read/only. Writes will receive EPERM *)
    | Send_flush  (** server supports Command.Flush *)
    | Send_fua    (** server supports NBD_CMD_FLAG_FUA *)
    | Rotational  (** let the client schedule I/O for a rotational medium *)
    | Send_trim   (** server supports Command.Trim *)
    | Send_write_zeroes (** server supports Command.WriteZeroes *)
 
  (** Per-export flags *)

  val to_string: t -> string
end

module GlobalFlag: sig
  (** During the protocol negotiation there are some defined flags used
      to choose protocol variants. These flags are sent by the server. *)

  type t =
    | Fixed_newstyle (** server supports the fixed newstyle protocol *)
    | No_zeroes      (** request to omit the 124 bytes of zeroes *)

  val to_string: t -> string
end

module ClientFlag: sig
  (** During the protocol negotiation there are some defined flags used
      to choose protocol variants. These flags are sent by the client. *)

  type t =
    | Fixed_newstyle (** client acknowledges use of fixed newstyle protocol *)
    | No_zeroes      (** client acknowledges omission of 124 bytes of zeroes *)

  val to_string: t -> string
end

module Option: sig
  (** In the 'newstyle' negotiation there is an opportunity for the client
      to negotiate options with the server. These are the known options. *)

  type t =
    | ExportName (** The client would like to connect to a given disk/export by
                     name *)
    | Abort      (** The client would like to quit. *)
    | List       (** The client would like to receive a list of known
                     disk/exports. *)
    | StartTLS   (** The client would like to protect the session with TLS. *)
    | Info (** The client would like to get details about an export with the
               given name *)
    | Go (** The client would like to connect to a given disk/export by name
             using NBD_OPT_GO *)
    | StructuredReply (** The client wishes to use structured replies during
                          the transmission phase *)
    | ListMetaContext (** List the metadata contexts that are available to the
                          client. *)
    | SetMetaContext (** Change the set of active metadata contexts. *)
    | Unknown of int32 (** This option is unknown to this implementation *)

  val to_string: t -> string
end

module OptionResponse: sig
  (** When the client sends an option request, the server must reply. *)

  type reply =
    | Ack (** Option acknowledged *)
    | Server (** A description of an export (in reponse to [List]) *)
    | Info (** A detailed description about an aspect of an export *)
    | MetaContext (** A description of a metadata context. *)
    | Unknown of int32 (** The response is unknown to this implementation. *)

  type error_reply =
    | Unsupported (** The option is unsupported *)
    | Policy (** The option is blocked by an admin policy *)
    | Invalid (** The option was invalid (i.e. the client is buggy) *)
    | Platform (** The option is not supported in this platform. *)
    | TlsReqd (** The option is not allowed when the connection is not using TLS. *)
    | UnknownExport
    | Shutdown
    | BlockSizeReqd
    | TooBig
    | Unknown of int32 (** The response is unknown to this implementation. *)

  type t = (reply, error_reply) result

  val to_string: t -> string
end

module OptionError : sig
  (** An error response to an option sent in the option haggling phase, with an
   * optional error message suitable for display to the user. *)

  type t = OptionResponse.error_reply * string option

  val to_string : t -> string
end

module Info : sig
  (** Information types (NBD_INFO). These are used in information requests and
      responses. *)

  type t =
    | Export (** Basic information about an export *)
    | Name (** Represents the server's canonical name of the export. *)
    | Description (** A description of the export *)
    | BlockSize (** Represents the server's advertised block size constraints *)
    | Unknown of int

  val to_string : t -> string
  val of_int : int -> t
  val to_int : t -> int
end

module StructuredReplyType : sig
  type t =
    | None
    | OffsetData
    | OffsetHole
    | BlockStatus
    | Error
    | ErrorOffset
    | Unknown of int

  val to_string : t -> string
end

module StructuredReplyFlag : sig
  type t =
    | Done

  val to_string : t -> string
end

module AllocationMetadataFlag : sig
  type t =
    | Hole
    | Zero
  val to_string : t -> string
  val of_int32 : int32 -> t list
  val to_int32 : t list -> int32
end

(** {2 Unmarshalling and marshalling messages} *)
(** The input buffers do not need to be prezeroed. *)

(** {3 Handshake} *)

module Announcement: sig
  (** The server sends an initial greeting when the connectino is opened. It
      can be of two main types: the original [V1] and a 'newstyle' [V2]. *)

  type t = [ `V1 | `V2 ]

  val sizeof: int

  val marshal: Cstruct.t -> t -> unit
  val unmarshal: Cstruct.t -> (t, exn) result
end

module Negotiate: sig
  (** The initial greeting sent by the server *)

  type v1 = {
    size: int64; (** The size of the disk *)
    flags: PerExportFlag.t list; (** Flags associated with the disk *)
  }
  (** The original [V1] protocol supports only one disk. *)

  type v2 = GlobalFlag.t list
  (** The 'newstyle' [V2] protocol supports an option negotiation
      phase and a number of sub-options [GlobalFlag.t]s *)

  type t =
    | V1 of v1
    | V2 of v2
    (** The initial greeting sent by the server *)

  val to_string: t -> string

  val sizeof: Announcement.t -> int

  val marshal: Cstruct.t -> t -> unit
  val unmarshal: Cstruct.t -> Announcement.t -> (t, exn) result
end

module NegotiateResponse: sig
  (** The client's initial response to the server's greeting *)

  type t = ClientFlag.t list
  (** The client can send some flags, in response to flags set by the server. *)

  val sizeof: int

  val marshal: Cstruct.t -> t -> unit

  val unmarshal: Cstruct.t -> t
end

(** {3 Option negotiation} *)

module OptionRequestHeader: sig
  (** Every option the client requests has the same header. *)

  type t = {
    ty: Option.t; (** The option type *)
    length: int32; (** The length of the option data *)
  }
  (** The header of an option request sent by the client *)

  val sizeof: int

  val marshal: Cstruct.t -> t -> unit
  val unmarshal: Cstruct.t -> (t, exn) result
end

module ExportName: sig
  (** An ExportName option payload *)

  type t = string
  (** The name of the export the client wishes to connect to *)

  val sizeof: t -> int

  val marshal: Cstruct.t -> t -> unit
end

module DiskInfo: sig
  (** Details about the export chosen by the client, sent in response
      to an [ExportName] option. *)

  type t = {
    size: int64; (** The size of the disk in bytes *)
    flags: PerExportFlag.t list; (** Transmission flags associated with the disk *)
  }
  (** Details about the export chosen by the client. *)

  val sizeof: int

  val unmarshal: Cstruct.t -> (t, exn) result
  val marshal: Cstruct.t -> t -> unit
end

module MetaContextRequest: sig
  (** A list of queries to list or set the meta contexts *)

  type t = string * string list

  val sizeof: t -> int

  val marshal: Cstruct.t -> t -> unit
end

module InfoRequest: sig
  (** Payload of an information request (NBD_OPT_INFO) option.
      Sent by the client to get details about the export. *)

  type t = {
    export : string;
    requests : Info.t list;
  }
  val sizeof: t -> int
  val marshal: Cstruct.t -> t -> unit
  val unmarshal: Cstruct.t -> t
end


module OptionResponseHeader: sig
  (** The server sends a response to every option request sent by the
      client (except [ExportName] which is followed by a [DiskInfo].
      This is the header of the response. *)

  type t = {
    request_type: Option.t; (** The option type requested *)
    response_type: OptionResponse.t; (** The response code *)
    length: int32; (** The length of the payload associated with the response *)
  }
  (** The header of the response sent by the server in response to
      the client requesting an option. *)

  val sizeof: int

  val to_string: t -> string

  val unmarshal: Cstruct.t -> (t, exn) result
  val marshal: Cstruct.t -> t -> unit
end

module Server: sig
  (** In response to a [List] option, the server sends a number of
      [Server] responses and then finally an [Ack] *)

  type t = {
    name: string; (** The name of an available disk. *)
  }
  (** A reponse to a [List] option. Note this option is repeated, once
      per available disk. *)

  val sizeof: t -> int

  val unmarshal: Cstruct.t -> (t, exn) result
end

module MetaContext: sig
  (** In response to a [SetMetaContext] or [ListMetaContext] option, the server
      sends a number of [MetaContext] responses and then finally an [Ack] *)

  type t = int32 * string
  (** A description of a metadata context. A pair of NBD metadat context ID and
      metadata context name *)

  val unmarshal: Cstruct.t -> t
end

module InfoResponse : sig
  (** NBD_REP_INFO: A detailed description about an aspect of an export. *)

  type block_size = {
    min: int32;
    preferred: int32;
    max: int32;
  }
  type t =
    | Export of DiskInfo.t
    | Name of string
    | Description of string
    | BlockSize of block_size

  val to_string : t -> string
  val sizeof : t -> int
  val marshal : Cstruct.t -> t -> unit

  val unmarshal : Cstruct.t -> t option
  (** Returns None for unknown information types *)
end

val default_client_blocksize : InfoResponse.block_size
(** The default block sizes a client desiring maximum interoperability should
    use, as defined by the NBD protocol. *)


(** {3 Transmission phase} *)

module Request: sig
  (** After the negotation phase, clients send I/O requests to the server. *)

  type t = {
    ty : Command.t; (** The command type *)
    handle : int64; (** A unique handle used to match requests with responses.*)
    from : int64;   (** The start of the data region *)
    len : int32;    (** The length of the data region *)
  }
  (** An I/O request sent by the client to the server. *)

  val to_string: t -> string

  val sizeof: int

  val marshal: Cstruct.t -> t -> unit
  val unmarshal: Cstruct.t -> (t, exn) result
end

module Reply: sig
  (** A reply sent from the server in response to a [Request]. Note
      these arrive out-of-order. *)

  type t = {
    error : (unit, Error.t) result; (** Success or failure of the request *)
    handle : int64; (** The unique id in the [Request] *)
  }

  val to_string: t -> string

  val sizeof: int

  val marshal: Cstruct.t -> t -> unit
  val unmarshal: Cstruct.t -> (t, exn) result
end

module StructuredReplyChunk : sig
  (** A structured reply chunk sent from the server in response to a [Request].
      Note: replies may not be sent in the same order as requests, and
      structured reply chunks from one request may be interleaved with reply
      messages from other requests. *)

  type t = {
    flags : StructuredReplyFlag.t list;
    reply_type : StructuredReplyType.t;
    handle : int64;
    length : int32;
  }

  val to_string: t -> string

  val sizeof: int

  val marshal: Cstruct.t -> t -> unit
  val unmarshal: Cstruct.t -> (t, exn) result

  module OffsetDataChunk : sig
    type t = int64
    val sizeof : int
    val marshal : Cstruct.t -> t -> unit
    val unmarshal : Cstruct.t -> t
  end

  module OffsetHoleChunk : sig
    type t = {
      offset : int64;
      hole_size : int32;
    }

    val to_string : t -> string

    val sizeof : int
    val marshal : Cstruct.t -> t -> unit
    val unmarshal : Cstruct.t -> t
  end

  module BlockStatusChunk : sig
    type descriptor = {
      length : int32;
      status_flags : int32;
    }
    type t = {
      meta_context_id : int32;
      descriptors : descriptor list;
    }

    val to_string : t -> string

    val sizeof : t -> int
    val marshal : Cstruct.t -> t -> unit
    val unmarshal : Cstruct.t -> t
  end

  module ErrorChunk : sig
    type t = {
      error : Error.t;
      message : string;
    }

    val to_string : t -> string

    val sizeof : t -> int
    val marshal : Cstruct.t -> t -> unit
    val unmarshal : Cstruct.t -> t
  end

  module ErrorOffsetChunk : sig
    type t = {
      error : Error.t;
      message : string;
      offset : int64;
    }

    val to_string : t -> string

    val sizeof : t -> int
    val marshal : Cstruct.t -> t -> unit
    val unmarshal : Cstruct.t -> t
  end
end

module GenericReply : sig
  (** A reply message that is either a simple or a structured reply. *)

  type t =
    | Simple of Reply.t
    | Structured of StructuredReplyChunk.t

  val to_string : t -> string

  type reply_type =
    | SimpleReply
    | StructuredReply

  val get_reply_type : int32 -> (reply_type, exn) result

  val sizeof_magic : int

  val get_magic : Cstruct.t -> int32


  val sizeof : reply_type -> int

  val unmarshal : reply_type -> Cstruct.t -> (t, exn) result
end
