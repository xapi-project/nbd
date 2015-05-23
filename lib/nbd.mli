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

module Error: sig
  type t = [
  | `EPERM  (** Operation not permitted *)
  | `EIO    (** Input/output error *)
  | `ENOMEM (** Cannot allocate memory *)
  | `EINVAL (** Invalid argument *)
  | `ENOSPC (** No space left on device *)
  | `Unknown of int32
  ] with sexp
  (** Defined error codes which can be returned in response to a request
      in the data-pushing phase. *)

  val to_string: t -> string

  val of_int32: int32 -> t
  val to_int32: t -> int32
end

module Command: sig
  type t =
  | Read
  | Write
  | Disc
  | Flush
  | Trim
  | Unknown of int32

  val to_string: t -> string
end

module PerExportFlag: sig
  type t =
  | Read_only   (** export is read/only. Writes will receive EPERM *)
  | Send_flush  (** server supports Command.Flush *)
  | Send_fua    (** server supports NBD_CMD_FLAG_FUA *)
  | Rotational  (** let the client schedule I/O for a rotational medium *)
  | Send_trim   (** server supports Command.Trim *)
  with sexp
  (** Per-export flags *)

  val to_string: t -> string

  val of_int32: int32 -> t list
  val to_int32: t list -> int32
end

module Option: sig
  type t =
    | ExportName
    | Abort
    | List
    | Unknown of int32


  val to_string: t -> string

  val of_int32: int32 -> t
  val to_int32: t -> int32
end

module OptionResponse: sig
  type t =
    | Ack
    | Server
    | Unsupported
    | Policy
    | Invalid
    | Platform
    | Unknown of int32

  val to_string: t -> string

  val of_int32: int32 -> t
  val to_int32: t -> int32
end

module Announcement: sig
  type t = [ `V1 | `V2 ] with sexp

  val sizeof: int

  val marshal: Cstruct.t -> t -> unit
  val unmarshal: Cstruct.t -> [ `Ok of t | `Error of exn ]
end

module Negotiate: sig

  type v1 = {
    size: int64;
    flags: PerExportFlag.t list;
  } with sexp

  type v2 = [ `NewStyle ] list with sexp

  type t =
    | V1 of v1
    | V2 of v2

  val to_string: t -> string

  val sizeof: Announcement.t -> int

  val marshal: Cstruct.t -> t -> unit
  val unmarshal: Cstruct.t -> Announcement.t -> [ `Ok of t | `Error of exn ]
end

module NegotiateResponse: sig
  type t = unit

  val sizeof: int

  val marshal: Cstruct.t -> unit

  val unmarshal: Cstruct.t -> unit
end

module OptionRequestHeader: sig
  type t = {
    ty: Option.t;
    length: int32;
  }

  val sizeof: int

  val marshal: Cstruct.t -> t -> unit
  val unmarshal: Cstruct.t -> [ `Ok of t | `Error of exn ]
end

module ExportName: sig
  type t = string

  val sizeof: t -> int

  val marshal: Cstruct.t -> t -> unit
end

module DiskInfo: sig
  type t = {
    size: int64;
    flags: PerExportFlag.t list;
  }

  val sizeof: int

  val unmarshal: Cstruct.t -> [ `Ok of t | `Error of exn ]
end

module OptionResponseHeader: sig
  type t = {
    request_type: Option.t;
    reply_type: OptionResponse.t;
    length: int32;
  } with sexp

  val sizeof: int

  val to_string: t -> string

  val unmarshal: Cstruct.t -> [ `Ok of t | `Error of exn ]
end

module Server: sig
  type t = {
    name: string;
  } with sexp

  val sizeof: t -> int

  val unmarshal: Cstruct.t -> [ `Ok of t | `Error of exn ]
end  

module Request: sig
  type t = {
    ty : Command.t;
    handle : int64;
    from : int64;
    len : int32;
  }

  val to_string: t -> string

  val sizeof: int

  val marshal: Cstruct.t -> t -> unit
  val unmarshal: Cstruct.t -> [ `Ok of t | `Error of exn ]
end

module Reply: sig
  type t = {
    error : [ `Ok of unit | `Error of Error.t ];
    handle : int64;
  } with sexp

  val to_string: t -> string

  val sizeof: int

  val marshal: Cstruct.t -> t -> unit
  val unmarshal: Cstruct.t -> [ `Ok of t | `Error of exn ]

end
