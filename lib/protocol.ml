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

(* NBD client library *)

open Sexplib.Std
open Result

(* We need to serialise/deserialise result values *)
type ('a, 'b) _result = [ `Ok of 'a | `Error of 'b ] [@@deriving sexp]
let result_of_sexp a b s = match _result_of_sexp a b s with
  | `Ok x -> Ok x | `Error y -> Error y
let sexp_of_result a b r =
  sexp_of__result a b (match r with Ok x -> `Ok x | Error y -> `Error y)

let _nbd_cmd_read = 0l
let _nbd_cmd_write = 1l
let _nbd_cmd_disc = 2l
let _nbd_cmd_flush = 3l
let _nbd_cmd_trim = 4l

let nbd_request_magic = 0x25609513l
let nbd_reply_magic = 0x67446698l
let nbd_structured_reply_magic = 0x668e33efl

let nbd_flag_has_flags = 1
let nbd_flag_read_only = 2
let nbd_flag_send_flush = 4
let nbd_flag_send_fua = 8
let nbd_flag_rotational = 16
let nbd_flag_send_trim = 32
let nbd_flag_send_write_zeroes = 64

let nbd_flag_fixed_newstyle = 1
let nbd_flag_no_zeroes = 2

let nbd_flag_c_fixed_newstyle = 1
let nbd_flag_c_no_zeroes = 2

let nbd_cmd_flag_fua = 1
let nbd_cmd_flag_no_hole = 2
let nbd_cmd_flag_df = 4
let nbd_cmd_flag_req_one = 8

let nbd_reply_flag_done = 1

(* Flags for the base:allocation metadata context *)
let nbd_state_hole = 1l
let nbd_state_zero = 3l

let zero buf =
  for i = 0 to Cstruct.len buf - 1 do
    Cstruct.set_uint8 buf i 0
  done

module PerExportFlag = struct
  type t =
    | Read_only
    | Send_flush
    | Send_fua
    | Rotational
    | Send_trim
    | Send_write_zeroes
  [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  let of_int32 x =
    let flags = Int32.to_int x in
    let is_set i mask = i land mask = mask in
    List.map snd
      (List.filter (fun (mask,_) -> is_set flags mask)
         [ nbd_flag_read_only, Read_only;
           nbd_flag_send_flush, Send_flush;
           nbd_flag_send_fua, Send_fua;
           nbd_flag_rotational, Rotational;
           nbd_flag_send_trim, Send_trim;
           nbd_flag_send_write_zeroes, Send_write_zeroes;
         ])

  let to_int flags =
    let one = function
      | Read_only -> nbd_flag_read_only
      | Send_flush -> nbd_flag_send_flush
      | Send_fua -> nbd_flag_send_fua
      | Rotational -> nbd_flag_rotational
      | Send_trim -> nbd_flag_send_trim
      | Send_write_zeroes -> nbd_flag_send_write_zeroes
    in
    List.fold_left (lor) nbd_flag_has_flags (List.map one flags)

  let to_int32 flags = Int32.of_int (to_int flags)
end

module GlobalFlag = struct
  type t =
    | Fixed_newstyle
    | No_zeroes
  [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  let of_int flags =
    let is_set i mask = i land mask = mask in
    List.map snd
      (List.filter (fun (mask,_) -> is_set flags mask)
         [ nbd_flag_fixed_newstyle, Fixed_newstyle;
           nbd_flag_no_zeroes, No_zeroes; ])

  let to_int flags =
    let one = function
      | Fixed_newstyle -> nbd_flag_fixed_newstyle
      | No_zeroes -> nbd_flag_no_zeroes in
    List.fold_left (lor) 0 (List.map one flags)

end

module ClientFlag = struct
  type t =
    | Fixed_newstyle
    | No_zeroes
  [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  let of_int32 flags =
    let flags = Int32.to_int flags in
    let is_set mask = mask land flags <> 0 in
    List.map snd
      (List.filter (fun (mask,_) -> is_set mask)
         [ nbd_flag_c_fixed_newstyle, Fixed_newstyle;
           nbd_flag_c_no_zeroes, No_zeroes; ])

  let to_int32 flags =
    let one = function
      | Fixed_newstyle -> nbd_flag_c_fixed_newstyle
      | No_zeroes -> nbd_flag_c_no_zeroes in
    Int32.of_int (List.fold_left (lor) 0 (List.map one flags))

end

module Error = struct
  type t = [
    | `EPERM
    | `EIO
    | `ENOMEM
    | `EINVAL
    | `ENOSPC
    | `EOVERFLOW
    | `ESHUTDOWN
    | `Unknown of int32
  ] [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  let of_int32 = function
    | 1l -> `EPERM
    | 5l -> `EIO
    | 12l -> `ENOMEM
    | 22l -> `EINVAL
    | 28l -> `ENOSPC
    | 75l -> `EOVERFLOW
    | 108l -> `ESHUTDOWN
    | x -> `Unknown x

  let to_int32 = function
    | `EPERM -> 1l
    | `EIO -> 5l
    | `ENOMEM -> 12l
    | `EINVAL -> 22l
    | `ENOSPC -> 28l
    | `EOVERFLOW -> 75l
    | `ESHUTDOWN -> 108l
    | `Unknown x -> x
end

module Command = struct
  type t =
    | Read
    | Write
    | Disc
    | Flush
    | Trim
    | WriteZeroes
    | BlockStatus
    | Unknown of int
  [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  let of_int = function
    | 0 -> Read
    | 1 -> Write
    | 2 -> Disc
    | 3 -> Flush
    | 4 -> Trim
    | 6 -> WriteZeroes
    | 7 -> BlockStatus
    | c  -> Unknown c

  let to_int = function
    | Read -> 0
    | Write -> 1
    | Disc -> 2
    | Flush -> 3
    | Trim -> 4
    | WriteZeroes -> 6
    | BlockStatus -> 7
    | Unknown c -> c

end

module CommandFlag = struct
  type t =
    | Fua
    | NoHole
    | Df
    | ReqOne
  [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  let of_int flags =
    let is_set mask = mask land flags <> 0 in
    List.map
      snd
      (List.filter
         (fun (mask,_) -> is_set mask)
         [ nbd_cmd_flag_fua, Fua
         ; nbd_cmd_flag_no_hole, NoHole
         ; nbd_cmd_flag_df, Df
         ; nbd_cmd_flag_req_one, ReqOne
         ]
      )

  let to_int flags =
    let one = function
      | Fua -> nbd_cmd_flag_fua
      | NoHole -> nbd_cmd_flag_no_hole
      | Df -> nbd_cmd_flag_df
      | ReqOne -> nbd_cmd_flag_req_one
    in
    List.fold_left (lor) 0 (List.map one flags)

end

module Option = struct
  type t =
    | ExportName
    | Abort
    | List
    | StartTLS
    | Info
    | Go
    | StructuredReply
    | ListMetaContext
    | SetMetaContext
    | Unknown of int32
  [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  let of_int32 = function
    | 1l -> ExportName
    | 2l -> Abort
    | 3l -> List
    (* 4 is not in use in the NBD protocol. *)
    | 5l -> StartTLS
    | 6l -> Info
    | 7l -> Go
    | 8l -> StructuredReply
    | 9l -> ListMetaContext
    | 10l -> SetMetaContext
    | c -> Unknown c

  let to_int32 = function
    | ExportName -> 1l
    | Abort -> 2l
    | List -> 3l
    | StartTLS -> 5l
    | Info -> 6l
    | Go -> 7l
    | StructuredReply -> 8l
    | ListMetaContext -> 9l
    | SetMetaContext -> 10l
    | Unknown c -> c
end

module OptionResponse = struct
  type reply =
    | Ack
    | Server
    | Info
    | MetaContext
    | Unknown of int32
  [@@deriving sexp]

  type error_reply =
    | Unsupported
    | Policy
    | Invalid
    | Platform
    | TlsReqd
    | UnknownExport
    | Shutdown
    | BlockSizeReqd
    | TooBig
    | Unknown of int32
  [@@deriving sexp]

  type t = (reply, error_reply) result
  [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  let bit_31 = -2147483648l

  let of_int32 = function
    | 1l -> Ok Ack
    | 2l -> Ok Server
    | 3l -> Ok Info
    | 4l -> Ok MetaContext
    | -2147483647l -> Error Unsupported
    | -2147483646l -> Error Policy
    | -2147483645l -> Error Invalid
    | -2147483644l -> Error Platform
    | -2147483643l -> Error TlsReqd
    | -2147483642l -> Error UnknownExport
    | -2147483641l -> Error Shutdown
    | -2147483640l -> Error BlockSizeReqd
    | -2147483639l -> Error TooBig
      (** Errors are denoted by having bit 31 set *)
    | x when Int32.logand x bit_31 = bit_31 -> Error (Unknown x)
    | x -> Ok (Unknown x)

  let to_int32 = function
    | Ok Ack -> 1l
    | Ok Server -> 2l
    | Ok Info -> 3l
    | Ok MetaContext -> 4l
    | Error Unsupported -> -2147483647l
    | Error Policy -> -2147483646l
    | Error Invalid -> -2147483645l
    | Error Platform -> -2147483644l
    | Error TlsReqd -> -2147483643l
    | Error UnknownExport -> -2147483642l
    | Error Shutdown      -> -2147483641l
    | Error BlockSizeReqd -> -2147483640l
    | Error TooBig        -> -2147483639l
    | Error (Unknown x) | Ok (Unknown x) -> x

end

module OptionError = struct
  type t = OptionResponse.error_reply * string option [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)
end

module Info = struct
  type t =
    | Export
    | Name
    | Description
    | BlockSize
    | Unknown of int
  [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  let of_int = function
    | 0 -> Export
    | 1 -> Name
    | 2 -> Description
    | 3 -> BlockSize
    | x -> Unknown x

  let to_int = function
    | Export      -> 0
    | Name        -> 1
    | Description -> 2
    | BlockSize   -> 3
    | Unknown x   -> x
end

module StructuredReplyType = struct
  type t =
    | None
    | OffsetData
    | OffsetHole
    | BlockStatus
    | Error
    | ErrorOffset
    | Unknown of int
  [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  let of_int = function
    | 0 -> None
    | 1 -> OffsetData
    | 2 -> OffsetHole
    | 5 -> BlockStatus
    | 32769 -> Error
    | 32770 -> ErrorOffset
    | x -> Unknown x

  let to_int = function
    | None -> 0
    | OffsetData -> 1
    | OffsetHole -> 2
    | BlockStatus -> 5
    | Error -> 32769
    | ErrorOffset -> 32770
    | Unknown x -> x
end

module StructuredReplyFlag = struct
  type t =
    | Done
  [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  let of_int flags =
    let is_set mask = mask land flags <> 0 in
    List.map snd
      (List.filter (fun (mask,_) -> is_set mask)
         [ nbd_reply_flag_done, Done ])

  let to_int flags =
    let one = function
      | Done -> nbd_reply_flag_done
    in
    List.fold_left (lor) 0 (List.map one flags)
end

module AllocationMetadataFlag = struct
  type t =
    | Hole
    | Zero
  [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  let of_int32 flags =
    let is_set mask = Int32.logand mask flags <> 0l in
    List.filter
      (fun (mask,_) -> is_set mask)
      [ nbd_state_hole, Hole
      ; nbd_state_zero, Zero
      ]
    |> List.map snd

  let to_int32 flags =
    let one = function
      | Hole -> nbd_state_hole
      | Zero -> nbd_state_zero
    in
    List.fold_left (Int32.logor) 0l (List.map one flags)
end


(* Sent by the server to the client which includes an initial
   protocol choice *)
module Announcement = struct
  type t = [ `V1 | `V2 ] [@@deriving sexp]

  [%%cstruct
    type t = {
      passwd: uint8_t [@len 8];
      magic:  uint64_t;
    } [@@big_endian]
  ]

  let sizeof = sizeof_t

  let expected_passwd = "NBDMAGIC"

  let v1_magic = 0x00420281861253L
  let v2_magic = 0x49484156454F5054L (* Ascii encoding of "IHAVEOPT" *)

  let marshal buf t =
    set_t_passwd expected_passwd 0 buf;
    set_t_magic buf (match t with `V1 -> v1_magic | `V2 -> v2_magic)

  let unmarshal buf =
    let passwd = Cstruct.to_string (get_t_passwd buf) in
    if passwd <> expected_passwd
    then Error (Failure "Bad magic in negotiate")
    else
      let magic = get_t_magic buf in
      if magic = v1_magic
      then Ok `V1
      else
      if magic = v2_magic
      then Ok `V2
      else Error (Failure (Printf.sprintf "Bad magic; expected %Ld or %Ld got %Ld" v1_magic v2_magic magic))
end

module Negotiate = struct
  type v1 = {
    size: int64;
    flags: PerExportFlag.t list;
  } [@@deriving sexp]

  type v2 = GlobalFlag.t list [@@deriving sexp]

  type t =
    | V1 of v1
    | V2 of v2
  [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  [%%cstruct
    type v1 = {
      size:    uint64_t;
      flags:   uint32_t;
      padding: uint8_t [@len 124];
    } [@@big_endian]
  ]

  [%%cstruct
    type v2 = {
      flags: uint16_t;
    } [@@big_endian]
  ]
  let sizeof = function
    | `V1 -> sizeof_v1
    | `V2 -> sizeof_v2

  let marshal buf t =
    zero buf;
    match t with
    | V1 t ->
      set_v1_size buf t.size;
      set_v1_flags buf (PerExportFlag.to_int32 t.flags);
    | V2 t ->
      set_v2_flags buf (GlobalFlag.to_int t)

  let unmarshal buf t =
    match t with
    | `V1 ->
      let size = get_v1_size buf in
      let flags = PerExportFlag.of_int32 (get_v1_flags buf) in
      Ok (V1 { size; flags })
    | `V2 ->
      let flags = GlobalFlag.of_int (get_v2_flags buf) in
      Ok (V2 flags)
end

module NegotiateResponse = struct
  type t = ClientFlag.t list [@@deriving sexp]

  let sizeof = 4

  let marshal buf t =
    Cstruct.BE.set_uint32 buf 0 (ClientFlag.to_int32 t)

  let unmarshal buf =
    ClientFlag.of_int32 (Cstruct.BE.get_uint32 buf 0)

end

(* In the 'new' and 'new fixed' protocols, options are preceeded by
   a common header which includes a type and a length. *)
module OptionRequestHeader = struct
  type t = {
    ty: Option.t;
    length: int32;
  } [@@deriving sexp]

  [%%cstruct
    type t = {
      magic:  uint64_t;
      ty:     uint32_t;
      length: uint32_t;
    } [@@big_endian]
  ]
  let sizeof = sizeof_t

  let marshal buf t =
    set_t_magic buf Announcement.v2_magic;
    set_t_ty buf (Option.to_int32 t.ty);
    set_t_length buf t.length

  let unmarshal buf =
    let open Rresult in
    let magic = get_t_magic buf in
    ( if Announcement.v2_magic <> magic
      then Error (Failure (Printf.sprintf "Bad reply magic: expected %Ld, got %Ld" Announcement.v2_magic magic))
      else Ok () ) >>= fun () ->
    let ty = Option.of_int32 (get_t_ty buf) in
    let length = get_t_length buf in
    Ok { ty; length }
end

(* This is the option sent by the client to select a particular disk
   export. *)
module ExportName = struct
  type t = string [@@deriving sexp]

  let sizeof = String.length

  let marshal buf x =
    Cstruct.blit_from_string x 0 buf 0 (String.length x)
end

(* In both the 'new' style handshake and the 'fixed new' style handshake,
   the server will reply to an ExportName option with either a connection
   close or a DiskInfo: *)
module DiskInfo = struct
  type t = {
    size: int64;
    flags: PerExportFlag.t list
  } [@@deriving sexp]

  [%%cstruct
    type t = {
      size:    uint64_t;
      flags:   uint16_t;
      padding: uint8_t  [@len 124];
    } [@@big_endian]
  ]
  let sizeof = sizeof_t

  let unmarshal buf =
    let size = get_t_size buf in
    let flags = PerExportFlag.of_int32 (Int32.of_int (get_t_flags buf)) in
    Ok { size; flags }

  let marshal buf t =
    set_t_size buf t.size;
    set_t_flags buf (PerExportFlag.to_int t.flags)
end

module MetaContextRequest = struct
  type t = string * string list

  let sizeof (exportname, queries) =
    List.fold_left
      (fun size s -> size + 4 (* length *) + (String.length s))
      (4 (* export name length *) + (String.length exportname) + 4 (* query number *))
      queries

  let marshal buf (exportname, queries) =
    Cstruct.BE.set_uint32 buf 0 (String.length exportname |> Int32.of_int);
    Cstruct.blit_from_string exportname 0 buf 4 (String.length exportname);
    let buf = Cstruct.shift buf ((String.length exportname) + 4) in
    Cstruct.BE.set_uint32 buf 0 (List.length queries |> Int32.of_int);
    let buf = Cstruct.shift buf 4 in
    List.fold_left
      (fun buf s ->
         Cstruct.BE.set_uint32 buf 0 (String.length s |> Int32.of_int);
         Cstruct.blit_from_string s 0 buf 4 (String.length s);
         Cstruct.shift buf ((String.length s) + 4)
      )
      buf
      queries
    |> ignore
end

module InfoRequest = struct
  type t = {
    export : string;
    requests : Info.t list;
  } [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  let sizeof t =
    4 (* export name len *) + (String.length t.export) + 2 (* number of requests *) + 2 * (List.length t.requests)

  let marshal buf t =
    let export_name_len = String.length t.export in
    Cstruct.BE.set_uint32 buf 0 (export_name_len |> Int32.of_int);
    Cstruct.blit_from_string t.export 0 buf 4 export_name_len;
    let buf = Cstruct.shift buf (4 + export_name_len) in
    Cstruct.BE.set_uint16 buf 0 (List.length t.requests);
    let buf = Cstruct.shift buf 2 in
    List.iteri
      (fun i r -> Cstruct.BE.set_uint16 buf (i * 2) (Info.to_int r))
      t.requests

  let unmarshal buf =
    let export_name_len = Cstruct.BE.get_uint32 buf 0 |> Int32.to_int in
    let export = Cstruct.sub buf 4 export_name_len |> Cstruct.to_string in
    let buf = Cstruct.shift buf (4 + export_name_len) in
    let num_requests = Cstruct.BE.get_uint16 buf 0 in
    let buf = Cstruct.shift buf 2 in
    let rec loop acc buf = function
      | 0 -> List.rev acc
      | n ->
        let i = Cstruct.BE.get_uint16 buf 0 |> Info.of_int in
        loop (i::acc) (Cstruct.shift buf 2) (n - 1)
    in
    let requests = loop [] buf num_requests in
    { export; requests }
end


(* In the 'fixed new' style handshake, all options apart from ExportName
   should result in reply packets as follows: *)
module OptionResponseHeader = struct
  [%%cstruct
    type t = {
      magic:         uint64_t;
      request_type:  uint32_t;
      response_type: uint32_t;
      length:        uint32_t;
    } [@@big_endian]
  ]
  type t = {
    request_type: Option.t;
    response_type: OptionResponse.t;
    length: int32;
  } [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  let sizeof = sizeof_t

  let expected_magic = 0x3e889045565a9L

  let unmarshal buf =
    let open Rresult in
    let magic = get_t_magic buf in
    ( if expected_magic <> magic
      then Error (Failure (Printf.sprintf "Bad reply magic: expected %Ld, got %Ld" expected_magic magic))
      else Ok () ) >>= fun () ->
    let request_type = Option.of_int32 (get_t_request_type buf) in
    let response_type = OptionResponse.of_int32 (get_t_response_type buf) in
    let length = get_t_length buf in
    Ok { request_type; response_type; length }

  let marshal buf t =
    set_t_magic buf expected_magic;
    set_t_request_type buf (Option.to_int32 t.request_type);
    set_t_response_type buf (OptionResponse.to_int32 t.response_type);
    set_t_length buf t.length
end

(* A description of an export, sent in response to a List option *)
module Server = struct
  type t = {
    name: string;
  } [@@deriving sexp]

  [%%cstruct
    type t = {
      length: uint32_t;
    } [@@big_endian]
  ]
  let sizeof t = sizeof_t + (String.length t.name)

  let unmarshal buf =
    let length = Int32.to_int (get_t_length buf) in
    let buf = Cstruct.shift buf sizeof_t in
    let name = Cstruct.(to_string (sub buf 0 length)) in
    Ok { name }
end

module MetaContext = struct
  type t = int32 * string

  let unmarshal buf =
    let meta_context_id = Cstruct.BE.get_uint32 buf 0 in
    let meta_context_name = Cstruct.shift buf 4 |> Cstruct.to_string in
    (meta_context_id, meta_context_name)
end

module InfoResponse = struct
  type block_size = {
    min: int32;
    preferred: int32;
    max: int32;
  } [@@deriving sexp]
  type t =
    | Export of DiskInfo.t
    | Name of string
    | Description of string
    | BlockSize of block_size
  [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  let sizeof = function
    | Export _ -> 2 (* type *) + 8 (* size *) + 2 (* flags *)
    | Name s | Description s -> 2 (* type *) + (String.length s)
    | BlockSize _ -> 2 (* type *) + 4 (* min *) + 4 (* preferred *) + 4 (* max *)

  let get_type = function
    | Export _ -> Info.Export
    | Name _ -> Info.Name
    | Description _ -> Info.Description
    | BlockSize _ -> Info.BlockSize

  let marshal buf t =
    Cstruct.BE.set_uint16 buf 0 (get_type t |> Info.to_int);
    let buf = Cstruct.shift buf 2 in
    match t with
    | Export DiskInfo.{size; flags} ->
      Cstruct.BE.set_uint64 buf 0 size;
      Cstruct.BE.set_uint16 buf 8 (PerExportFlag.to_int flags)
    | Name s | Description s ->
      Cstruct.blit_from_string s 0 buf 0 (String.length s)
    | BlockSize {min; preferred; max} ->
      Cstruct.BE.set_uint32 buf 0 min;
      Cstruct.BE.set_uint32 buf 4 preferred;
      Cstruct.BE.set_uint32 buf 8 max

  let unmarshal buf =
    let ty = Cstruct.BE.get_uint16 buf 0 |> Info.of_int in
    let buf = Cstruct.shift buf 2 in
    match ty with
    | Export ->
      let size = Cstruct.BE.get_uint64 buf 0 in
      let flags = Cstruct.BE.get_uint16 buf 8 |> Int32.of_int |> PerExportFlag.of_int32 in
      Some (Export {size; flags})
    | Name -> Some (Name (Cstruct.to_string buf))
    | Description -> Some (Description (Cstruct.to_string buf))
    | BlockSize ->
      let min = Cstruct.BE.get_uint32 buf 0 in
      let preferred = Cstruct.BE.get_uint32 buf 4 in
      let max = Cstruct.BE.get_uint32 buf 8 in
      Some (BlockSize {min; preferred; max})
    | Unknown _ ->
      (* A client "MUST ignore information types that it does not recognize" *)
      None
end

let default_client_blocksize =
  InfoResponse.{min = 512l; preferred = 4096l; max = 33_554_432l}

module Request = struct
  type t = {
    command_flags : CommandFlag.t list;
    ty : Command.t;
    handle : int64;
    from : int64;
    len : int32
  } [@@deriving sexp]

  let to_string t =
    Printf.sprintf "{ Command = %s; handle = %Ld; from = %Ld; len = %ld }"
      (Command.to_string t.ty) t.handle t.from t.len

  [%%cstruct
    type t = {
      magic:  uint32_t;
      flags:  uint16_t;
      ty:     uint16_t;
      handle: uint64_t;
      from:   uint64_t;
      len:    uint32_t;
    } [@@big_endian]
  ]
  let unmarshal (buf: Cstruct.t) =
    let open Rresult in
    let magic = get_t_magic buf in
    ( if nbd_request_magic <> magic
      then Error (Failure (Printf.sprintf "Bad request magic: expected %ld, got %ld" magic nbd_request_magic))
      else Ok () ) >>= fun () ->
    let command_flags = CommandFlag.of_int (get_t_flags buf) in
    let ty = Command.of_int (get_t_ty buf) in
    let handle = get_t_handle buf in
    let from = get_t_from buf in
    let len = get_t_len buf in
    Ok { command_flags; ty; handle; from; len }

  let sizeof = sizeof_t

  let marshal (buf: Cstruct.t) t =
    set_t_magic buf nbd_request_magic;
    set_t_flags buf (CommandFlag.to_int t.command_flags);
    set_t_ty buf (Command.to_int t.ty);
    set_t_handle buf t.handle;
    set_t_from buf t.from;
    set_t_len buf t.len
end

module Reply = struct
  type t = {
    error : (unit, Error.t) result;
    handle : int64;
  } [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  [%%cstruct
    type t = {
      magic:  uint32_t;
      error:  uint32_t;
      handle: uint64_t;
    } [@@big_endian]
  ]
  let unmarshal (buf: Cstruct.t) =
    let open Rresult in
    let magic = get_t_magic buf in
    ( if nbd_reply_magic <> magic
      then Error (Failure (Printf.sprintf "Bad reply magic: expected %ld, got %ld" nbd_reply_magic magic))
      else Ok () ) >>= fun () ->
    let error = get_t_error buf in
    let error = if error = 0l then Ok () else Error (Error.of_int32 error) in
    let handle = get_t_handle buf in
    Ok { error; handle }

  let sizeof = sizeof_t

  let marshal (buf: Cstruct.t) t =
    set_t_magic buf nbd_reply_magic;
    let error = match t.error with
      | Ok () -> 0l
      | Error e -> Error.to_int32 e in
    set_t_error buf error;
    set_t_handle buf t.handle
end

module StructuredReplyChunk = struct
  type t = {
    flags : StructuredReplyFlag.t list;
    reply_type : StructuredReplyType.t;
    handle : int64;
    length : int32;
  } [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  [%%cstruct
    type t = {
      magic: uint32_t;
      flags: uint16_t;
      reply_type: uint16_t;
      handle: uint64_t;
      length: uint32_t;
    } [@@big_endian]
  ]
  let unmarshal buf =
    let open Rresult in
    let magic = get_t_magic buf in
    ( if nbd_structured_reply_magic <> magic
      then Error (Failure (Printf.sprintf "Bad structured reply magic: expected %ld, got %ld" nbd_structured_reply_magic magic))
      else Ok () ) >>= fun () ->
    let flags = get_t_flags buf |> StructuredReplyFlag.of_int in
    let reply_type = get_t_reply_type buf |> StructuredReplyType.of_int in
    let handle = get_t_handle buf in
    let length = get_t_length buf in
    Ok { flags; reply_type; handle; length }

  let sizeof = sizeof_t

  let marshal buf t =
    set_t_magic buf nbd_structured_reply_magic;
    set_t_flags buf (t.flags |> StructuredReplyFlag.to_int);
    set_t_reply_type buf (t.reply_type |> StructuredReplyType.to_int);
    set_t_handle buf t.handle;
    set_t_length buf t.length;

  module OffsetDataChunk = struct
    type t = int64
    let sizeof = 8
    let marshal buf t = Cstruct.BE.set_uint64 buf 0 t
    let unmarshal buf = Cstruct.BE.get_uint64 buf 0
  end

  module OffsetHoleChunk = struct
    type t = {
      offset : int64;
      hole_size : int32;
    } [@@deriving sexp]

    [%%cstruct
      type t = {
        offset : uint64_t;
        hole_size : uint32_t;
      } [@@big_endian]
    ]

    let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

    let sizeof = sizeof_t

    let marshal buf t =
      set_t_offset buf t.offset;
      set_t_hole_size buf t.hole_size

    let unmarshal buf =
      let offset = get_t_offset buf in
      let hole_size = get_t_hole_size buf in
      { offset; hole_size }
  end

  module BlockStatusChunk = struct
    type descriptor = {
      length : int32;
      status_flags : int32;
    } [@@deriving sexp]
    type t = {
      meta_context_id : int32;
      descriptors : descriptor list;
    } [@@deriving sexp]

    let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

    let sizeof t =
      4 (* meta context id *) + (4 (* length *) + 4 (* status flags *)) * (List.length t.descriptors)

    let marshal buf t =
      Cstruct.BE.set_uint32 buf 0 t.meta_context_id;
      let buf = Cstruct.shift buf 4 in
      List.iteri
        (fun i {length; status_flags} ->
           Cstruct.BE.set_uint32 buf (8 * i) length;
           Cstruct.BE.set_uint32 buf (8 * i + 4) status_flags)
        t.descriptors

    let unmarshal buf =
      let meta_context_id = Cstruct.BE.get_uint32 buf 0 in
      let buf = Cstruct.shift buf 4 in
      let rec loop acc buf = match Cstruct.len buf with
        | 0 -> List.rev acc
        | _ ->
          let length = Cstruct.BE.get_uint32 buf 0 in
          let status_flags = Cstruct.BE.get_uint32 buf 4 in
          loop ({length; status_flags}::acc) (Cstruct.shift buf 8)
      in
      let descriptors = loop [] buf in
      { meta_context_id; descriptors }
  end

  module ErrorChunk = struct
    type t = {
      error : Error.t;
      message : string;
    } [@@deriving sexp]

    let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

    [%%cstruct
      type t = {
        error: uint32_t;
        msg_len: uint16_t;
      } [@@big_endian]
    ]

    let sizeof t = sizeof_t + (String.length t.message)

    let marshal buf t =
      set_t_error buf (t.error |> Error.to_int32);
      set_t_msg_len buf (String.length t.message);
      Cstruct.blit_from_string t.message 0 buf sizeof_t (String.length t.message)

    let unmarshal buf =
      let error = get_t_error buf |> Error.of_int32 in
      let msg_len = get_t_msg_len buf in
      let message = Cstruct.sub buf sizeof_t msg_len |> Cstruct.to_string in
      { error; message }
  end

  module ErrorOffsetChunk = struct
    type t = {
      error : Error.t;
      message : string;
      offset : int64;
    } [@@deriving sexp]

    let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

    let sizeof t = 4 + 2 + (String.length t.message) + 8

    let marshal buf t =
      Cstruct.BE.set_uint32 buf 0 (t.error |> Error.to_int32);
      Cstruct.BE.set_uint16 buf 4 (String.length t.message);
      Cstruct.blit_from_string t.message 0 buf 6 (String.length t.message);
      Cstruct.BE.set_uint64 buf (6 + String.length t.message) t.offset

    let unmarshal buf =
      let error = Cstruct.BE.get_uint32 buf 0 |> Error.of_int32 in
      let msg_len = Cstruct.BE.get_uint16 buf 4 in
      let message = Cstruct.sub buf 6 msg_len |> Cstruct.to_string in
      let offset = Cstruct.BE.get_uint64 buf (6 + msg_len) in
      { error; message; offset }
  end

  (** XXX We could also detect unkown errors with bit 15 set, but it's not
   * mandatory: "If the client receives an unknown or unexpected type with bit
   * 15 set, it MUST consider the current reply as errored, but MAY continue
   * transmission unless it detects that message length is too large to fit
   * within the length specified by the header. For all other messages with
   * unknown or unexpected type or inconsistent contents, the client MUST
   * initiate a hard disconnect." *)
end

module GenericReply = struct
  type t =
    | Simple of Reply.t
    | Structured of StructuredReplyChunk.t
  [@@deriving sexp]

  let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)

  type reply_type =
    | SimpleReply
    | StructuredReply

  let get_reply_type magic =
    match magic with
    | m when m = nbd_reply_magic -> Ok SimpleReply
    | m when m = nbd_structured_reply_magic -> Ok StructuredReply
    | m ->
      Error (Failure (Printf.sprintf "Bad reply magic: expected %ld or %ld, got %ld" nbd_reply_magic nbd_structured_reply_magic m))

  let sizeof_magic = 4

  let get_magic buf = Cstruct.BE.get_uint32 buf 0

  let sizeof = function
    | SimpleReply -> Reply.sizeof
    | StructuredReply -> StructuredReplyChunk.sizeof

  let unmarshal t buf =
    let open Rresult in
    match t with
    | SimpleReply ->
      Reply.unmarshal buf >>| fun r -> Simple r
    | StructuredReply ->
      StructuredReplyChunk.unmarshal buf >>| fun r -> Structured r
end
