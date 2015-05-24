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
open Nbd_lwt_channel

type size = int64

let get_handle =
  let next = ref 0L in
  fun () ->
    let this = !next in
    next := Int64.succ !next;
    this

module NbdRpc = struct
  type transport = channel
  type id = int64
  type request_hdr = Request.t
  type request_body = Cstruct.t option
  type response_hdr = Reply.t
  type response_body = [ `Ok of Cstruct.t option | `Error of Error.t ]

  let recv_hdr sock =
    let buf = Cstruct.create 16 in
    lwt () = sock.read buf in
    match Reply.unmarshal buf with
    | `Ok x -> return (Some x.Reply.handle, x)
    | `Error e -> fail e

  let recv_body sock req_hdr res_hdr =
    match res_hdr.Reply.error with
    | `Error e -> return (`Error e)
    | `Ok () ->
      begin match req_hdr.Request.ty with
      | Command.Read -> 
        (* TODO: use a page-aligned memory allocator *)
        let expected = Int32.to_int req_hdr.Request.len in
        let data = Cstruct.create expected in
        sock.read data
        >>= fun () ->
        return (`Ok (Some data))
     | _ -> return (`Ok None)
     end

  let send_one sock req_hdr req_body =
    let buf = Cstruct.create Request.sizeof in
    Request.marshal buf req_hdr;
    lwt () = sock.write buf in
    match req_body with
    | None -> return ()
    | Some data ->
      let expected = Cstruct.len data in
      sock.write data

  let id_of_request req = req.Request.handle

  let handle_unrequested_packet t reply =
    fail (Failure (Printf.sprintf "Unexpected response from server: %s" (Reply.to_string reply)))
end

module Mux = Nbd_lwt_mux.Mux(NbdRpc)

type t = Mux.client

let list channel =
  let buf = Cstruct.create Announcement.sizeof in
  channel.read buf
  >>= fun () ->
  match Announcement.unmarshal buf with
  | `Error e -> fail e
  | `Ok kind ->
    let buf = Cstruct.create (Negotiate.sizeof kind) in
    channel.read buf
    >>= fun () ->
    begin match Negotiate.unmarshal buf kind with
    | `Error e -> fail e
    | `Ok (Negotiate.V1 x) ->
      return (`Error `Unsupported)
    | `Ok (Negotiate.V2 x) ->
      let buf = Cstruct.create NegotiateResponse.sizeof in
      let flags = if List.mem GlobalFlag.Fixed_newstyle x then [ ClientFlag.Fixed_newstyle ] else [] in
      NegotiateResponse.marshal buf flags;
      channel.write buf
      >>= fun () ->
      let buf = Cstruct.create OptionRequestHeader.sizeof in
      OptionRequestHeader.(marshal buf { ty = Option.List; length = 0l });
      channel.write buf
      >>= fun () ->
      let buf = Cstruct.create OptionResponseHeader.sizeof in
      let rec loop acc =
        channel.read buf
        >>= fun () ->
        match OptionResponseHeader.unmarshal buf with
        | `Error e -> fail e
        | `Ok { OptionResponseHeader.reply_type = OptionResponse.Ack } -> return (`Ok acc)
        | `Ok { OptionResponseHeader.reply_type = OptionResponse.Policy } ->
          return (`Error `Policy)
        | `Ok { OptionResponseHeader.reply_type = OptionResponse.Server; length } ->
          let buf' = Cstruct.create (Int32.to_int length) in
          channel.read buf'
          >>= fun () ->
          begin match Server.unmarshal buf' with
          | `Ok server ->
            loop (server.Server.name :: acc)
          | `Error e -> fail e
          end in
      loop []
    end

let negotiate channel export =
  let buf = Cstruct.create Announcement.sizeof in
  channel.read buf
  >>= fun () ->
  match Announcement.unmarshal buf with
  | `Error e -> fail e
  | `Ok kind ->
    let buf = Cstruct.create (Negotiate.sizeof kind) in
    channel.read buf
    >>= fun () ->
    begin match Negotiate.unmarshal buf kind with
    | `Error e -> fail e
    | `Ok (Negotiate.V1 x) ->
      Mux.create channel
      >>= fun t ->
      return (t, x.Negotiate.size, x.Negotiate.flags)
    | `Ok (Negotiate.V2 x) ->
      let buf = Cstruct.create NegotiateResponse.sizeof in
      let flags = if List.mem GlobalFlag.Fixed_newstyle x then [ ClientFlag.Fixed_newstyle ] else [] in
      NegotiateResponse.marshal buf flags;
      channel.write buf
      >>= fun () ->
      let buf = Cstruct.create OptionRequestHeader.sizeof in
      OptionRequestHeader.(marshal buf { ty = Option.ExportName; length = Int32.of_int (String.length export) });
      channel.write buf
      >>= fun () ->
      let buf = Cstruct.create (ExportName.sizeof export) in
      ExportName.marshal buf export;
      channel.write buf
      >>= fun () ->
      let buf = Cstruct.create DiskInfo.sizeof in
      channel.read buf
      >>= fun () ->
      begin match DiskInfo.unmarshal buf with
      | `Error e -> fail e
      | `Ok x ->
        Mux.create channel
        >>= fun t ->
        return (t, x.DiskInfo.size, x.DiskInfo.flags)
      | `Ok _ ->
        fail (Failure "Expected to receive size and flags from the server")
      end
    end

let write t data from =
  let handle = get_handle () in
  let req_hdr = {
    Request.ty = Command.Write;
    handle; from;
    len = Int32.of_int (Cstruct.len data)
  } in
  Mux.rpc req_hdr (Some data) t
  >>= function
  | _, `Ok _ -> return (`Ok ())
  | _, `Error e -> return (`Error e)

let read t from len =
  let handle = get_handle () in
  let req_hdr = {
    Request.ty = Command.Read;
    handle; from; len
  } in
  let req_body = None in
  Mux.rpc req_hdr req_body t
  >>= function
    | _, `Ok (Some res) -> return (`Ok res)
    | _, `Ok None -> return (`Ok (Cstruct.create 0))
    | _, `Error e -> return (`Error e)
