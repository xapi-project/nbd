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

module Flag: sig
  type t =
  | Read_only
  | Send_flush
  | Send_fua
  | Rotational
  | Send_trim

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

module Announcement: sig
  type t = [ `V1 | `V2 ] with sexp

  val sizeof: int

  val marshal: Cstruct.t -> t -> unit
  val unmarshal: Cstruct.t -> [ `Ok of t | `Error of exn ]
end

module Negotiate: sig

  type v1 = {
    size: int64;
    flags: Flag.t list;
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

module ExportName: sig
  type t = string

  val sizeof: t -> int

  val marshal: Cstruct.t -> t -> unit
end

module OptionResult: sig
  type t = {
    size: int64;
    flags: Flag.t list;
  }

  val sizeof: int

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
    error : int32;
    handle : int64;
  }

  val to_string: t -> string

  val sizeof: int

  val marshal: Cstruct.t -> t -> unit
  val unmarshal: Cstruct.t -> [ `Ok of t | `Error of exn ]

end
