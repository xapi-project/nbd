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

open Nbd
open Lwt

type size = int64

let get_handle =
  let next = ref 0L in
  fun () ->
    let this = !next in
    next := Int64.succ !next;
    this

let really_read sock buf n =
  let rec loop acc sock buf ofs len = 
    lwt n = Lwt_bytes.read sock buf ofs len in
    let len = len - n in
    let acc = acc + n in
    if len = 0 || n = 0
    then return acc
    else loop acc sock buf (ofs + n) len in
  lwt actual = loop 0 sock buf 0 n in
  if actual <> n
  then fail (Failure (Printf.sprintf "Short read; expected %d, got %d" n actual))
  else return ()

let really_write sock buf expected =
  lwt actual = Lwt_bytes.write sock buf 0 expected in
  if expected <> actual
  then fail (Failure (Printf.sprintf "Short write; expected %d got %d" expected actual))
  else return ()

module NbdRpc = struct
  type transport = Lwt_unix.file_descr
  type id = int64
  type request_hdr = Request.t
  type request_body = Lwt_bytes.t option
  type response_hdr = Reply.t
  type response_body = Lwt_bytes.t option

  let recv_hdr sock =
    let buf = Lwt_bytes.create 16 in
    lwt () = really_read sock buf 16 in
    match Reply.unmarshal (Cstruct.of_bigarray buf) with
    | Result.Ok x -> return (Some x.Reply.handle, x)
    | Result.Error e -> fail e

  let recv_body sock req_hdr res_hdr =
    if res_hdr.Reply.error <> 0l
    then fail (Failure (Printf.sprintf "Error %ld returned" res_hdr.Reply.error))
    else match req_hdr.Request.ty with
    | Command.Read -> 
      (* TODO: use a page-aligned memory allocator *)
      let expected = Int32.to_int req_hdr.Request.len in
      let data = Lwt_bytes.create expected in
      lwt () = really_read sock data expected in
      return (Some data)
   | _ -> return None

  let send_one sock req_hdr req_body =
    let buf = Lwt_bytes.create Request.sizeof in
    Request.marshal (Cstruct.of_bigarray buf) req_hdr;
    lwt () = really_write sock buf Request.sizeof in
    match req_body with
    | None -> return ()
    | Some data ->
      let expected = Bigarray.Array1.dim data in
      really_write sock data expected

  let id_of_request req = req.Request.handle

  let handle_unrequested_packet t reply =
    return ()
end

module Mux = Lwt_mux.Mux(NbdRpc)

type t = Mux.client

let negotiate sock =
  let buf = Lwt_bytes.create Negotiate.sizeof in
  lwt () = really_read sock buf Negotiate.sizeof in
  let buf = Cstruct.of_bigarray buf in
  let passwd = Cstruct.to_string (Negotiate.get_t_passwd buf) in
  if passwd <> Negotiate.expected_passwd
  then fail (Failure "Bad magic in negotiate")
  else
    let magic = Negotiate.get_t_magic buf in
    if magic = Negotiate.opts_magic
    then fail (Failure "Unhandled opts_magic")
    else if magic <> Negotiate.cliserv_magic
    then fail (Failure (Printf.sprintf "Bad magic; expected %Ld got %Ld" Negotiate.cliserv_magic magic))
    else
      let size = Negotiate.get_t_size buf in
      let flags = Flag.of_int32 (Negotiate.get_t_flags buf) in
      lwt t = Mux.create sock in
      return (t, size, flags)

let connect hostname port =
  let socket = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  lwt host_info = Lwt_unix.gethostbyname hostname in
  let server_address = host_info.Lwt_unix.h_addr_list.(0) in
  lwt () = Lwt_unix.connect socket (Lwt_unix.ADDR_INET (server_address, port)) in
  negotiate socket

let write t data from =
  let handle = get_handle () in
  let req_hdr = {
    Request.ty = Command.Write;
    handle; from;
    len = Int32.of_int (Bigarray.Array1.dim data)
  } in
  lwt _ = Mux.rpc req_hdr (Some data) t in
  return ()

let read t from len =
  let handle = get_handle () in
  let req_hdr = {
    Request.ty = Command.Read;
    handle; from; len
  } in
  let req_body = None in
  lwt res = Mux.rpc req_hdr req_body t in
  match res with
    | (_,Some res) -> return res
    | _ -> fail (Failure "No response!?")
