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

let nbd_cmd_read = 0l
let nbd_cmd_write = 1l
let nbd_cmd_disc = 2l
let nbd_cmd_flush = 3l
let nbd_cmd_trim = 4l

let nbd_request_magic = 0x25609513l
let nbd_reply_magic = 0x67446698l

let nbd_flag_has_flags = 1
let nbd_flag_read_only = 2
let nbd_flag_send_flush = 4
let nbd_flag_send_fua = 8
let nbd_flag_rotational = 16
let nbd_flag_send_trim = 32

let init_passwd = "NBDMAGIC"
let opts_magic = 0x49484156454F5054L
let cliserv_magic = 0x00420281861253L

module Flag = struct
  type t =
    | Read_only
    | Send_flush
    | Send_fua
    | Rotational
    | Send_trim


  let of_flags flags =
    let is_set i mask = i land mask = mask in
      List.map snd 
        (List.filter (fun (mask,_) -> is_set flags mask)
          [ nbd_flag_read_only, Read_only;
            nbd_flag_send_flush, Send_flush;
            nbd_flag_send_fua, Send_fua;
            nbd_flag_rotational, Rotational;
            nbd_flag_send_trim, Send_trim; ])
		
end

module Command = struct
  type t =
    | Read
    | Write
    | Disc
    | Flush
    | Trim

  let of_int32 = function 
  | 0l -> Read 
  | 1l -> Write 
  | 2l -> Disc 
  | 3l -> Flush 
  | 4l -> Trim

  let to_int32 = function 
  | Read -> 0l 
  | Write -> 1l 
  | Disc -> 2l 
  | Flush -> 3l 
  | Trim -> 4l

end

module Request = struct
  type t = {
    ty : cmd;
    handle : int64;
    from : int64;
    len : int32
  }

  cstruct t {
    uint32_t magic;
    uint32_t ty;
    uint64_t handle;
    uint64_t from;
    uint32_t len
  } as big_endian

  let unmarshal (buf: Cstruct.t) =
    let open Result in
    let magic = get_t_magic buf in
    ( if nbd_request_magic <> magic
      then fail (Failure (Printf.sprintf "Bad request magic: expected %s, got %s" magic nbd_request_magic))
      else return () ) >>= fun () ->
    let ty = Command.of_int32 (get_t_ty buf) in
    let handle = get_t_handle buf in
    let from = get_t_from buf in
    let len = get_t_len buf in
    return { ty; handle; from; len }

  let sizeof = sizeof_t

  let marshal (buf: Cstruct.t) t =
    set_t_magic buf nbd_request_magic;
    set_t_ty buf (Command.to_int32 t.ty);
    set_t_handle buf t.handle;
    set_t_from buf t.from;
    set_t_len buf t.len
end
	
module Reply = struct
  type t = {
    error : int32;
    handle : int64;
  }

  cstruct t {
    uint32_t magic;
    uint32_t error;
    uint64_t handle
  } as big_endian

  let unmarshal (buf: Cstruct.t) =
    let open Result in
    let magic = get_t_magic buf in
    ( if nbd_reply_magic <> magic
      then fail (Failure (Printf.sprintf "Bad reply magic: expected %s, got %s" magic nbd_reply_magic))
      else return () ) >>= fun () ->
    let error = get_t_error buf in
    let handle = get_t_handle buf in
    return { error; handle }

  let sizeof = sizeof_t

  let marshal (buf: Cstruct.t) t =
    set_t_magic buf nbd_reply_magic;
    set_t_error buf t.error;
    set_t_handle buf t.handle
end
