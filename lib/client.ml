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
open Protocol
open Channel

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
  type response_body = Cstruct.t list

  let recv_hdr sock =
    let buf = Cstruct.create 16 in
    sock.read buf
    >>= fun () ->
    match Reply.unmarshal buf with
    | Result.Ok x -> return (Some x.Reply.handle, x)
    | Result.Error e -> fail e

  let recv_body sock req_hdr res_hdr response_body =
    match res_hdr.Reply.error with
    | `Error e -> return (Result.Error e)
    | `Ok () ->
      begin match req_hdr.Request.ty with
        | Command.Read ->
          (* TODO: use a page-aligned memory allocator *)
          Lwt_list.iter_s sock.read response_body
          >>= fun () ->
          return (Result.Ok ())
        | _ -> return (Result.Ok ())
      end

  let send_one sock req_hdr req_body =
    let buf = Cstruct.create Request.sizeof in
    Request.marshal buf req_hdr;
    sock.write buf
    >>= fun () ->
    match req_body with
    | None -> return ()
    | Some data ->
      sock.write data

  let id_of_request req = req.Request.handle

  let handle_unrequested_packet t reply =
    fail (Failure (Printf.sprintf "Unexpected response from server: %s" (Reply.to_string reply)))
end

module Rpc = Mux.Make(NbdRpc)

type info = {
  read_write: bool;
  sector_size: int;
  size_sectors: int64;
}

type t = {
  client: Rpc.client;
  info: info;
  mutable disconnected: bool;
}

type id = unit

let make channel size_bytes flags =
  Rpc.create channel
  >>= fun client ->
  let read_write = not (List.mem PerExportFlag.Read_only flags) in
  let sector_size = 1 in (* Note: NBD has no notion of a sector *)
  let size_sectors = size_bytes in
  let info = { read_write; sector_size; size_sectors } in
  let disconnected = false in
  return { client; info; disconnected }

let list channel =
  let buf = Cstruct.create Announcement.sizeof in
  channel.read buf
  >>= fun () ->
  match Announcement.unmarshal buf with
  | Result.Error e -> fail e
  | Result.Ok kind ->
    let buf = Cstruct.create (Negotiate.sizeof kind) in
    channel.read buf
    >>= fun () ->
    begin match Negotiate.unmarshal buf kind with
      | Result.Error e -> fail e
      | Result.Ok (Negotiate.V1 x) ->
        return (Result.Error `Unsupported)
      | Result.Ok (Negotiate.V2 x) ->
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
          | Result.Error e -> fail e
          | Result.Ok { OptionResponseHeader.response_type = OptionResponse.Ack } -> return (Result.Ok acc)
          | Result.Ok { OptionResponseHeader.response_type = OptionResponse.Policy } ->
            return (Result.Error `Policy)
          | Result.Ok { OptionResponseHeader.response_type = OptionResponse.Server; length } ->
            let buf' = Cstruct.create (Int32.to_int length) in
            channel.read buf'
            >>= fun () ->
            begin match Server.unmarshal buf' with
              | Result.Ok server ->
                loop (server.Server.name :: acc)
              | Result.Error e -> fail e
            end
          | Result.Ok _ ->
            fail (Failure "Server's OptionResponse had an invalid type") in
        loop []
    end

let negotiate channel export =
  let buf = Cstruct.create Announcement.sizeof in
  channel.read buf
  >>= fun () ->
  match Announcement.unmarshal buf with
  | Result.Error e -> fail e
  | Result.Ok kind ->
    let buf = Cstruct.create (Negotiate.sizeof kind) in
    channel.read buf
    >>= fun () ->
    begin match Negotiate.unmarshal buf kind with
      | Result.Error e -> fail e
      | Result.Ok (Negotiate.V1 x) ->
        make channel x.Negotiate.size x.Negotiate.flags
        >>= fun t ->
        return (t, x.Negotiate.size, x.Negotiate.flags)
      | Result.Ok (Negotiate.V2 x) ->
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
          | Result.Error e -> fail e
          | Result.Ok x ->
            make channel x.DiskInfo.size x.DiskInfo.flags
            >>= fun t ->
            return (t, x.DiskInfo.size, x.DiskInfo.flags)
        end
    end

type 'a io = 'a Lwt.t

type page_aligned_buffer = Cstruct.t

type error = [
  | `Unknown of string
  | `Unimplemented
  | `Is_read_only
  | `Disconnected
]

let get_info t = return t.info

let write_one t from buffer =
  let handle = get_handle () in
  let req_hdr = {
    Request.ty = Command.Write;
    handle; from;
    len = Int32.of_int (Cstruct.len buffer)
  } in
  Rpc.rpc req_hdr (Some buffer) [] t.client

let write t from buffers =
  if t.disconnected
  then return (`Error `Disconnected)
  else begin
    let rec loop from = function
      | [] -> return (Result.Ok ())
      | b :: bs ->
        begin write_one t from b
          >>= function
          | Result.Ok () -> loop Int64.(add from (of_int (Cstruct.len b))) bs
          | Result.Error e -> return (Result.Error e)
        end in
    loop from buffers
    >>= function
    | Result.Ok x -> return (`Ok x)
    | Result.Error e -> return (`Error (`Unknown (Printf.sprintf "NBD client: %s" (Error.to_string e))))
  end

let read t from buffers =
  if t.disconnected
  then return (`Error `Disconnected)
  else begin
    let handle = get_handle () in
    let len = Int32.of_int @@ List.fold_left (+) 0 @@ List.map Cstruct.len buffers in
    let req_hdr = {
      Request.ty = Command.Read;
      handle; from; len
    } in
    let req_body = None in
    Rpc.rpc req_hdr req_body buffers t.client
    >>= function
    | Result.Ok x -> return (`Ok x)
    | Result.Error e -> return (`Error (`Unknown (Printf.sprintf "NBD client: %s" (Error.to_string e))))
  end

let disconnect t =
  t.disconnected <- true;
  return ()
