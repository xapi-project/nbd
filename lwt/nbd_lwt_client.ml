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
open Nbd
open Nbd_lwt_common

type size = int64

let get_handle =
  let next = ref 0L in
  fun () ->
    let this = !next in
    next := Int64.succ !next;
    this

module NbdRpc = struct
  type transport = Lwt_unix.file_descr
  type id = int64
  type request_hdr = Request.t
  type request_body = Cstruct.t option
  type response_hdr = Reply.t
  type response_body = Cstruct.t option

  let recv_hdr sock =
    let buf = Cstruct.create 16 in
    lwt () = really_read sock buf in
    match Reply.unmarshal buf with
    | Result.Ok x -> return (Some x.Reply.handle, x)
    | Result.Error e -> fail e

  let recv_body sock req_hdr res_hdr =
    if res_hdr.Reply.error <> 0l
    then fail (Failure (Printf.sprintf "Error %ld returned" res_hdr.Reply.error))
    else match req_hdr.Request.ty with
    | Command.Read -> 
      (* TODO: use a page-aligned memory allocator *)
      let expected = Int32.to_int req_hdr.Request.len in
      let data = Cstruct.create expected in
      lwt () = really_read sock data in
      return (Some data)
   | _ -> return None

  let send_one sock req_hdr req_body =
    let buf = Cstruct.create Request.sizeof in
    Request.marshal buf req_hdr;
    lwt () = really_write sock buf in
    match req_body with
    | None -> return ()
    | Some data ->
      let expected = Cstruct.len data in
      really_write sock data

  let id_of_request req = req.Request.handle

  let handle_unrequested_packet t reply =
    return ()
end

module Mux = Lwt_mux.Mux(NbdRpc)

type t = Mux.client

let negotiate sock =
  let buf = Cstruct.create Negotiate.sizeof in
  lwt () = really_read sock buf in
  match Negotiate.unmarshal buf with
  | Result.Error e -> fail e
  | Result.Ok x ->
    lwt t = Mux.create sock in
    return (t, x.Negotiate.size, x.Negotiate.flags)

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
    len = Int32.of_int (Cstruct.len data)
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
