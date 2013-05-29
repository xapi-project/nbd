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

type flag = | NBD_read_only
			| NBD_send_flush
			| NBD_send_fua
			| NBD_rotational
			| NBD_send_trim

type cmd = | NBD_cmd_read
		   | NBD_cmd_write
		   | NBD_cmd_disc
		   | NBD_cmd_flush
		   | NBD_cmd_trim

let flags_of_flags flags =
	let is_set i mask = i land mask = mask in
	List.map snd 
		(List.filter (fun (mask,_) -> is_set flags mask)
			[ nbd_flag_read_only, NBD_read_only;
			  nbd_flag_send_flush, NBD_send_flush;
			  nbd_flag_send_fua, NBD_send_fua;
			  nbd_flag_rotational, NBD_rotational;
			  nbd_flag_send_trim, NBD_send_trim; ])
		
module Request = struct
	type t = {
		ty : cmd;
		handle : int64;
		from : int64;
		len : int32
	}
end
	
module Reply = struct
	type t = {
		error : int32;
		handle : int64;
	}
end

let ty_of_int32 = function 
	| 0l -> NBD_cmd_read 
	| 1l -> NBD_cmd_write 
	| 2l -> NBD_cmd_disc 
	| 3l -> NBD_cmd_flush 
	| 4l -> NBD_cmd_trim

let int32_of_ty = function 
	| NBD_cmd_read -> 0l 
	| NBD_cmd_write -> 1l 
	| NBD_cmd_disc -> 2l 
	| NBD_cmd_flush -> 3l 
	| NBD_cmd_trim -> 4l

let parse_request req = 
	bitmatch req with
		| { magic : 32 : bigendian;
		    ty : 32 : bigendian;
			handle : 64 : bigendian;
			from : 64 : bigendian;
			len : 32 : bigendian } ->
			if magic <> nbd_request_magic then failwith "Bad magic in request";
			{ Request.ty = ty_of_int32 ty;
			  handle = handle;
			  from = from;
			  len = len; }
		|  { } -> failwith "Bad request"

let parse_reply reply =
	bitmatch reply with
		| { magic : 32 : bigendian;
		    err : 32 : bigendian;
			handle : 64 : bigendian } ->
			if magic <> nbd_reply_magic then failwith "Bad magic in reply";
			{ Reply.error = err;
			  handle = handle }
		| { } -> failwith "Bad reply"


let construct_request req =
	let bits = BITSTRING {
		nbd_request_magic : 32 : bigendian;
		(int32_of_ty req.Request.ty) : 32 : bigendian;
		req.Request.handle : 64 : bigendian;
		req.Request.from : 64 : bigendian;
		req.Request.len : 32 : bigendian }
	in Bitstring.string_of_bitstring bits

let construct_reply reply =
	let bits = BITSTRING {
		nbd_reply_magic : 32 : bigendian;
		reply.Reply.error : 32 : bigendian;
		reply.Reply.handle : 64 : bigendian }
	in Bitstring.string_of_bitstring bits

let get_int64 bs =
	bitmatch bs with 
		| { i : 64 : bigendian } -> i
		| { } -> failwith "Not an int64!"

let get_int32 bs =
	bitmatch bs with
		| { i : 32 : bigendian } -> i
		| { } -> failwith "Not an int32!"




		
