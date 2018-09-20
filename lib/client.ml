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

open Lwt.Infix
open Protocol
open Channel
open Result

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

  type request_hdr = Protocol.Request.t
  type response_hdr = Protocol.GenericReply.t

  let send_hdr chan =
    let buf = Cstruct.create 16 in
    chan.read buf

  let fail_on_error = function
    | Ok x -> Lwt.return x
    | Error e -> Lwt.fail e

  let send_hdr chan req_hdr =
    let buf = Cstruct.create Request.sizeof in
    Request.marshal buf req_hdr;
    chan.write buf

  let recv_hdr chan =
    let magic_buf = Cstruct.create GenericReply.sizeof_magic in
    chan.read magic_buf >>= fun () ->
    let magic = GenericReply.get_magic magic_buf in
    GenericReply.get_reply_type magic |> fail_on_error >>= fun reply_type ->
    let buf = Cstruct.create (GenericReply.sizeof reply_type - GenericReply.sizeof_magic) in
    chan.read buf >>= fun () ->
    let buf = Cstruct.append magic_buf buf in
    GenericReply.unmarshal reply_type buf |> fail_on_error

  let id_of_request r = r.Request.handle

  let id_of_response = function
    | GenericReply.Simple r -> r.Reply.handle
    | GenericReply.Structured r -> r.StructuredReplyChunk.handle

  let final_response = function
    | GenericReply.Simple _ -> true
    | GenericReply.Structured r ->
      List.mem StructuredReplyFlag.Done r.StructuredReplyChunk.flags
end

module Rpc = Mux.Make(NbdRpc)

type error = [ Mirage_block.error | `Protocol_error of Protocol.Error.t ]

type write_error = [ Mirage_block.write_error | `Protocol_error of Protocol.Error.t ]

let pp_error ppf = function
  | #Mirage_block.error as e -> Mirage_block.pp_error ppf e
  | `Protocol_error e -> Fmt.string ppf (Protocol.Error.to_string e)

let pp_write_error ppf = function
  | #Mirage_block.write_error as e -> Mirage_block.pp_write_error ppf e
  | `Protocol_error e -> Fmt.string ppf (Protocol.Error.to_string e)

type client = generic_channel

type t = {
  client: Rpc.client;
  info: Mirage_block.info;
  mutable disconnected: bool;
}

type id = unit

let make channel size_bytes flags =
  Rpc.create channel
  >>= fun client ->
  let read_write = not (List.mem PerExportFlag.Read_only flags) in
  let sector_size = 1 in (* Note: NBD has no notion of a sector *)
  let size_sectors = size_bytes in
  let info = { Mirage_block.read_write; sector_size; size_sectors } in
  let disconnected = false in
  Lwt.return { client; info; disconnected }

let read_error_msg channel payload_length =
  (* The protocol says: "All error replies MAY have some data set, in which
   * case that data is an error message string suitable for display to the
   * user." *)
  if payload_length > 0l then
    let buf = Cstruct.create (Int32.to_int payload_length) in
    channel.read buf >|= fun () ->
    Some (Cstruct.to_string buf)
  else Lwt.return None

let read_ack channel req =
  let buf = Cstruct.create OptionResponseHeader.sizeof in
  channel.read buf
  >>= fun () ->
  match OptionResponseHeader.unmarshal buf with
  | Error e -> Lwt.fail e
  | Ok OptionResponseHeader.{ request_type; response_type = Ok OptionResponse.Ack; length = 0l; _ } when request_type = req ->
    Lwt.return_ok ()
  | Ok OptionResponseHeader.{ request_type; response_type = Error resp; length; _ } when request_type = req ->
    read_error_msg channel length >>= fun error_msg ->
    Lwt.return_error (resp, error_msg)
  | Ok r -> Lwt.fail_with ("Server's returned invalid OptionResponse: " ^ (OptionResponseHeader.to_string r))

let abort channel =
  (* Send NBD_OPT_ABORT to terminate the option haggling *)
  let buf = Cstruct.create OptionRequestHeader.sizeof in
  OptionRequestHeader.(marshal buf { ty = Option.Abort; length = 0l });
  channel.write buf >>= fun () ->
  (* The NBD protocol says: "the client SHOULD gracefully handle the
   * server closing the connection after receiving an NBD_OPT_ABORT
   * without it sending a reply" *)
  Lwt.catch
    (fun () ->
       read_ack channel Option.Abort >>= function
       | Error (code, msg) ->
         Lwt_log_core.warning_f "Got error %s [%s] while reading ack from server after abort"
           (OptionResponse.to_string (Error code))
           (match msg with Some msg -> msg | None -> "")
       | Ok _ -> Lwt.return_unit
    )
    (fun exn -> Lwt_log_core.warning ~exn "Got exception while reading ack from server after abort")

let list channel =
  let buf = Cstruct.create Announcement.sizeof in
  channel.read buf
  >>= fun () ->
  match Announcement.unmarshal buf with
  | Error e -> Lwt.fail e
  | Ok kind ->
    let buf = Cstruct.create (Negotiate.sizeof kind) in
    channel.read buf
    >>= fun () ->
    begin match Negotiate.unmarshal buf kind with
      | Error e -> Lwt.fail e
      | Ok (Negotiate.V1 _) ->
        Lwt.return_error `Unsupported
      | Ok (Negotiate.V2 x) ->
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
          | Error e -> Lwt.fail e
          | Ok OptionResponseHeader.{ request_type = Option.List; response_type = Ok OptionResponse.Ack; _} -> Lwt.return_ok acc
          | Ok OptionResponseHeader.{ request_type = Option.List; response_type = Error OptionResponse.Policy; _} ->
            Lwt.return_error `Policy
          | Ok OptionResponseHeader.{ request_type = Option.List; response_type = Ok OptionResponse.Server;
                 length; _} ->
            let buf' = Cstruct.create (Int32.to_int length) in
            channel.read buf'
            >>= fun () ->
            begin match Server.unmarshal buf' with
              | Ok server ->
                loop (server.Server.name :: acc)
              | Error e -> Lwt.fail e
            end
          | Ok _ ->
            Lwt.fail_with "Server's OptionResponse had an invalid type" in
        loop [] >>= fun result ->
        abort channel >|= fun () ->
        result
    end

let receive_announcement channel =
  let buf = Cstruct.create Announcement.sizeof in
  channel.read buf
  >>= fun () ->
  match Announcement.unmarshal buf with
  | Error e -> Lwt.fail e
  | Ok kind ->
    let buf = Cstruct.create (Negotiate.sizeof kind) in
    channel.read buf
    >>= fun () ->
    begin match Negotiate.unmarshal buf kind with
      | Error e -> Lwt.fail e
      | Ok (Negotiate.V1 x) -> Lwt.return (`Oldstyle x)
      | Ok (Negotiate.V2 x) ->
        let variant = if List.mem GlobalFlag.Fixed_newstyle x then `Fixed else `NonFixed in
        let client_flags = match variant with `Fixed -> [ ClientFlag.Fixed_newstyle ] | `NonFixed -> [] in
        let buf = Cstruct.create NegotiateResponse.sizeof in
        NegotiateResponse.marshal buf client_flags;
        channel.write buf >>= fun () ->
        Lwt.return (`Newstyle (variant, x))
    end

let connect channel =
  receive_announcement channel >>= function
  | `Oldstyle _ -> Lwt.fail_with "Server uses oldstyle negotiation"
  | `Newstyle (`NonFixed, _) -> Lwt.fail_with "Server uses non-fixed newstyle negotiation"
  | `Newstyle (`Fixed, _handshake_flags) -> Lwt.return channel

let negotiate_structured_reply channel =
  let buf = Cstruct.create OptionRequestHeader.sizeof in
  OptionRequestHeader.(marshal buf { ty = Option.StructuredReply; length = 0l });
  channel.write buf >>= fun () ->
  read_ack channel Option.StructuredReply

let send_meta_context_option ty channel export queries =
  let buf = Cstruct.create OptionRequestHeader.sizeof in
  let length = MetaContextRequest.sizeof (export, queries) in
  OptionRequestHeader.(marshal buf { ty; length = length |> Int32.of_int });
  channel.write buf >>= fun () ->
  let buf = Cstruct.create length in
  MetaContextRequest.marshal buf (export, queries);
  channel.write buf >>= fun () ->
  let buf = Cstruct.create OptionResponseHeader.sizeof in
  let rec loop acc =
    channel.read buf
    >>= fun () ->
    match OptionResponseHeader.unmarshal buf with
    | Error e -> Lwt.fail e
    | Ok OptionResponseHeader.{ request_type; response_type = Ok OptionResponse.Ack; length = 0l } when request_type = ty->
      Lwt.return acc
    | Ok OptionResponseHeader.{ request_type; response_type = Ok OptionResponse.MetaContext; length } when request_type = ty->
      let buf = Cstruct.create (Int32.to_int length) in
      channel.read buf >>= fun () ->
      let meta_context = MetaContext.unmarshal buf in
      begin match acc with
        | Ok acc -> loop (Ok (meta_context :: acc))
        | Error _ -> Lwt.fail_with "send_meta_context_option: Error response must be last"
      end
    | Ok OptionResponseHeader.{ request_type; response_type = Error resp; length } when request_type = ty->
      read_error_msg channel length >>= fun error_msg ->
      Lwt.return_error (resp, error_msg)
    | Ok r -> Lwt.fail_with ("send_meta_context_option: Unexpected option response: " ^ (OptionResponseHeader.to_string r))
  in
  loop (Ok ([]))

let list_meta_contexts channel export queries =
  send_meta_context_option Option.ListMetaContext channel export queries >|= function
  | Ok l ->
    (** The NBD protocol currently says "The metadata context ID in these replies
        is reserved and SHOULD be set to zero; clients MUST disregard it." *)
    let _ids, contexts = List.split l in
    Ok contexts
  | Error _ as e -> e

let set_meta_contexts = send_meta_context_option Option.SetMetaContext

let send_info_request ty channel export requests =
  let request = InfoRequest.{ export; requests } in
  let length = InfoRequest.sizeof request in
  let buf = Cstruct.create OptionRequestHeader.sizeof in
  OptionRequestHeader.(marshal buf { ty; length = length |> Int32.of_int });
  channel.write buf >>= fun () ->
  let buf = Cstruct.create length in
  InfoRequest.marshal buf request;
  channel.write buf >>= fun () ->
  let buf = Cstruct.create OptionResponseHeader.sizeof in
  let rec loop acc =
    channel.read buf
    >>= fun () ->
    match OptionResponseHeader.unmarshal buf with
    | Error e -> Lwt.fail e
    | Ok OptionResponseHeader.{ request_type; response_type = Ok OptionResponse.Ack; length = 0l } when request_type = ty ->
      Lwt.return acc
    | Ok OptionResponseHeader.{ request_type; response_type = Ok OptionResponse.Info; length } when request_type = ty ->
      let buf = Cstruct.create (Int32.to_int length) in
      channel.read buf >>= fun () ->
      let info = InfoResponse.unmarshal buf in
      begin match acc with
        | Ok acc ->
          (* A client "MUST ignore information types that it does not recognize" *)
          let acc = match info with Some info -> info :: acc | None -> acc in
          loop (Ok acc)
        | Error _ -> Lwt.fail_with "send_meta_context_option: Error response must be last"
      end
    | Ok OptionResponseHeader.{ request_type = Option.Info; response_type = Error resp; length } ->
      read_error_msg channel length >>= fun error_msg ->
      Lwt.return_error (resp, error_msg)
    | Ok r -> Lwt.fail_with ("query_info: Unexpected option response: " ^ (OptionResponseHeader.to_string r))
  in
  loop (Ok ([]))

let query_info = send_info_request Option.Info

let send_export_name channel export =
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
    | Error e -> Lwt.fail e
    | Ok x ->
      make channel x.DiskInfo.size x.DiskInfo.flags >>= fun t ->
      Lwt.return (t, x)
  end

let rec find_map f = function
  | x::l -> begin
      match f x with
      | Some _ as res -> res
      | None -> find_map f l
    end
  | [] -> None

let request_export channel export =
  send_info_request Option.Go channel export [Info.BlockSize] >>= function
  | Ok l ->
    (match find_map (function InfoResponse.Export i -> Some i | _ -> None) l with
        | Some i -> Lwt.return i
        | None -> Lwt.fail_with "NBDProtocolError: no NBD_INFO_EXPORT provided")
    >>= fun export_info ->
    let block_sizes =
      match find_map (function InfoResponse.BlockSize x -> Some x | _ -> None) l with
      | Some x -> Some x
      | None -> None
    in
    make channel export_info.DiskInfo.size export_info.DiskInfo.flags >>= fun t ->
    Lwt.return_ok (t, export_info, block_sizes)
  | Error ((OptionResponse.(Unsupported | TlsReqd), _) as e) ->
    Lwt_log.debug ("Failed to use NBD_OPT_GO: " ^ (OptionError.to_string e)) >>= fun () ->
    send_export_name channel export >>= fun (t, x) ->
    Lwt.return_ok (t, x, None)
  | Error ((OptionResponse.(UnknownExport | Policy | Invalid | Platform | Shutdown | BlockSizeReqd | TooBig | Unknown _), _) as e) ->
    Lwt.return_error e

let negotiate channel export =
  receive_announcement channel >>= function
  | `Oldstyle x ->
    make channel x.Negotiate.size x.Negotiate.flags
    >>= fun t ->
    Lwt.return (t, x.Negotiate.size, x.Negotiate.flags)
  | `Newstyle (_variant, _handshake_flags) ->
    send_export_name channel export >>= fun (t, x) ->
    Lwt.return (t, x.DiskInfo.size, x.DiskInfo.flags)


type 'a io = 'a Lwt.t

type page_aligned_buffer = Cstruct.t

let get_info t = Lwt.return t.info

let handle_error_chunk chan length =
  let buf = Cstruct.create (Int32.to_int length) in
  chan.read buf >>= fun () ->
  let e = StructuredReplyChunk.ErrorChunk.unmarshal buf in
  (** TODO also return msg *)
  Lwt.return_error e.error

let handle_error_offset_chunk chan length =
  let buf = Cstruct.create (Int32.to_int length) in
  chan.read buf >>= fun () ->
  let e = StructuredReplyChunk.ErrorOffsetChunk.unmarshal buf in
  (** TODO also return msg & offset & and all errors from server *)
  Lwt.return_error e.error

let write_cmd t ty ~from ~len ~writefn =
  let handle = get_handle () in
  let req_hdr = {
    Request.ty = ty;
    handle; from;
    len
  } in
  Rpc.rpc
    t.client
    req_hdr
    writefn
    (fun res chan -> function
       | Simple { error = Ok (); _ } -> Lwt.return (Ok ())
       | Simple { error = Error _ as e; _ } -> Lwt.return e
       | Structured { reply_type = StructuredReplyType.None; length = 0l; flags; _ } when List.mem StructuredReplyFlag.Done flags ->
         (** Length MUST be 0, and this chunk type MUST always be used with the
          * NBD_REPLY_FLAG_DONE bit set. If no earlier error chunks were sent,
          * then this type implies that the overall client request is
          * successful. *)
         Lwt.return res
       | Structured { reply_type = StructuredReplyType.Error; length; _ } when length >= 6l ->
         handle_error_chunk chan length
       | Structured { reply_type = StructuredReplyType.ErrorOffset; length; _ } when length >= 14l ->
         handle_error_offset_chunk chan length
       | Structured { reply_type = StructuredReplyType.(None | Error | ErrorOffset); _ } ->
         Lwt.fail_with "NBDProtocolError: Invalid structured reply"
       | Structured { reply_type = StructuredReplyType.(Unknown _ | BlockStatus | OffsetData | OffsetHole); _ } ->
         Lwt.fail_with "NBDProtocolError: Unexpected structured reply"
    )
    (Ok ())

let write_one t from buffer =
  write_cmd t Command.Write ~from ~len:(Cstruct.len buffer |> Int32.of_int)
    ~writefn:(fun chan -> chan.write buffer)

let write t from buffers =
  if t.disconnected
  then Lwt.return_error `Disconnected
  else begin
    let rec loop from = function
      | [] -> Lwt.return_ok ()
      | b :: bs ->
        begin write_one t from b
          >>= function
          | Ok _ -> loop Int64.(add from (of_int (Cstruct.len b))) bs
          | Error e -> Lwt.return_error e
        end in
    loop from buffers
    >>= function
    | Error e -> Lwt.return_error (`Protocol_error e)
    | Ok () -> Lwt.return_ok ()
  end

let write_zeroes t from len =
  if t.disconnected
  then Lwt.return_error `Disconnected
  else begin
    write_cmd t Command.WriteZeroes ~from ~len ~writefn:(fun _ -> Lwt.return_unit)
    >>= function
    | Error e -> Lwt.return_error (`Protocol_error e)
    | Ok () -> Lwt.return_ok ()
  end

let read_chunked t from len read_callback =
  let handle = get_handle () in
  let req_hdr = {
    Request.ty = Command.Read;
    handle; from; len
  } in
  Rpc.rpc
    t.client
    req_hdr
    (fun _ -> Lwt.return_unit)
    (fun res chan -> function
       | Simple { error = Ok (); _ } ->
         read_callback chan from len >>= fun () ->
         Lwt.return (Ok ())
       | Simple { error = Error _ as e; _ } ->
         Lwt.return e
       | Structured { reply_type = StructuredReplyType.None; length = 0l; flags; _ } when List.mem StructuredReplyFlag.Done flags ->
         (** Length MUST be 0, and this chunk type MUST always be used with the
          * NBD_REPLY_FLAG_DONE bit set. If no earlier error chunks were sent,
          * then this type implies that the overall client request is
          * successful. *)
         Lwt.return res
       | Structured { reply_type = StructuredReplyType.OffsetData; length; _ } when length >= 9l ->
         let buf = Cstruct.create StructuredReplyChunk.OffsetDataChunk.sizeof in
         chan.read buf >>= fun () ->
         let offset = StructuredReplyChunk.OffsetDataChunk.unmarshal buf in
         let data_len = Int32.sub length 8l in
         read_callback chan offset data_len >>= fun () ->
         Lwt.return (Ok ())
       | Structured { reply_type = StructuredReplyType.OffsetHole; length = 12l; _ } ->
         let buf = Cstruct.create StructuredReplyChunk.OffsetHoleChunk.sizeof in
         chan.read buf >>= fun () ->
         let _t = StructuredReplyChunk.OffsetHoleChunk.unmarshal buf in
         Lwt.return (Ok ())
       | Structured { reply_type = StructuredReplyType.Error; length; _ } when length >= 6l ->
         handle_error_chunk chan length
       | Structured { reply_type = StructuredReplyType.ErrorOffset; length; _ } when length >= 14l ->
         handle_error_offset_chunk chan length
       | Structured { reply_type = StructuredReplyType.(None | OffsetData | OffsetHole | Error | ErrorOffset); _ } ->
         Lwt.fail_with "NBDProtocolError: Invalid structured reply"
       | Structured { reply_type = StructuredReplyType.(Unknown _ | BlockStatus); _ } ->
         Lwt.fail_with "NBDProtocolError: Unexpected structured reply"
    )
    (Ok ())

let read t from buffers =
  if t.disconnected
  then Lwt.return_error `Disconnected
  else begin
    let len = Int32.of_int @@ List.fold_left (+) 0 @@ List.map Cstruct.len buffers in
    let rec read_into_buffers buffers chan offset (length:int32) =
      let offset = Int64.sub offset from in
      match offset, length, buffers with
      | _, 0l, _ -> Lwt.return_unit
      | _, _l, [] -> Lwt.fail_with "Received too much data"
      | 0L, l, b::buffers ->
        if (Int32.to_int l) > Cstruct.len b then
          chan.read b >>= fun () ->
          read_into_buffers buffers chan 0L (Int32.sub l (Cstruct.len b |> Int32.of_int))
        else
          let b = Cstruct.sub b 0 (Int32.to_int l) in
          chan.read b
      | offset, l, b::buffers ->
        if offset > Int64.of_int (Cstruct.len b) then
          let offset = Int64.sub offset (Int64.of_int (Cstruct.len b)) in
          read_into_buffers buffers chan offset l
        else
          let b = Cstruct.shift b (Int64.to_int offset) in
          read_into_buffers (b::buffers) chan 0L l
    in
    read_chunked t from len (read_into_buffers buffers)
    >>= function
    | Error e -> Lwt.return_error (`Protocol_error e)
    | Ok _ -> Lwt.return_ok ()
  end

let query_block_status t from len =
  if t.disconnected
  then Lwt.return_error `Disconnected
  else begin
    let handle = get_handle () in
    let req_hdr = {
      Request.ty = Command.BlockStatus;
      handle; from; len;
    } in
    Rpc.rpc
      t.client
      req_hdr
      (fun _chan -> Lwt.return_unit (* no request payload *))
      (fun res chan -> function
         | Structured { reply_type = StructuredReplyType.None; length = 0l; flags; _ } when List.mem StructuredReplyFlag.Done flags ->
           (** Length MUST be 0, and this chunk type MUST always be used with the
            * NBD_REPLY_FLAG_DONE bit set. If no earlier error chunks were sent,
            * then this type implies that the overall client request is
            * successful. *)
           Lwt.return res
         | Structured { reply_type = StructuredReplyType.BlockStatus; length; _ } when length >= 12l ->
           let buf = Cstruct.create (Int32.to_int length) in
           chan.read buf >>= fun () ->
           let t = StructuredReplyChunk.BlockStatusChunk.unmarshal buf in
           begin match res with
             | Error _ as e -> Lwt.return e
             | Ok l -> Lwt.return (Ok (t :: l))
           end
         | Structured { reply_type = StructuredReplyType.Error; length; _ } when length >= 6l ->
           handle_error_chunk chan length
         | Structured { reply_type = StructuredReplyType.ErrorOffset; length; _ } when length >= 14l ->
           handle_error_offset_chunk chan length
         | Structured { reply_type = StructuredReplyType.(None | BlockStatus | Error | ErrorOffset); _ } as r ->
           Lwt.fail_with ("NBDProtocolError: Invalid structured reply" ^ (GenericReply.to_string r))
         | Simple _ | Structured { reply_type = StructuredReplyType.(Unknown _ | OffsetData | OffsetHole); _ } as r ->
           Lwt.fail_with ("NBDProtocolError: Unexpected reply: " ^ (GenericReply.to_string r))
      )
      (Ok ([]))
    >>= function
    | Error e -> Lwt.return_error (`Protocol_error e)
    | Ok _ as r -> Lwt.return r
  end

let disconnect t =
  t.disconnected <- true;
  Lwt.return ()
