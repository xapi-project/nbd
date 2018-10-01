(*
 * Copyright (C) 2011-2015 Citrix Inc
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

let project_url = "http://github.com/xapi-project/nbd"

open Lwt.Infix

module Device = struct
  type id = [
    | `Nbd of Uri.t
    | `Local of string
  ]
  type t = [
    | `Nbd of Nbd_lwt_unix.Client.t
    | `Local of Block.t
  ]
  type 'a io = 'a Lwt.t
  type page_aligned_buffer = Cstruct.t
  type error = [
      Mirage_block.error
    | `Protocol_error of Nbd.Protocol.Error.t
  ]
  type write_error = [
      Mirage_block.write_error
    | `Protocol_error of Nbd.Protocol.Error.t
  ]
  let pp_error ppf = function
    | #Mirage_block.error as e -> Mirage_block.pp_error ppf e
    | `Protocol_error e -> Fmt.string ppf (Nbd.Protocol.Error.to_string e)

  let pp_write_error ppf = function
    | #Mirage_block.write_error as e -> Mirage_block.pp_write_error ppf e
    | `Protocol_error e -> Fmt.string ppf (Nbd.Protocol.Error.to_string e)

  let connect uri = match Uri.scheme uri with
    | Some "file" ->
      let path = Uri.path uri in
      ( Block.connect path >|= fun x ->
        `Local x )
    | Some "nbd" ->
      begin match Uri.host uri with
        | Some host ->
          let port = match Uri.port uri with None -> 10809 | Some x -> x in
          Nbd_lwt_unix.connect host port
          >>= fun channel ->
          Nbd_lwt_unix.Client.negotiate channel (Uri.to_string uri)
          >>= fun (t, _, _) ->
          Lwt.return (`Nbd t)
        | None -> Lwt.fail_with "Cannot connect to nbd without a host"
      end
    | _ -> Lwt.fail_with "unknown scheme"

  type info = {
    read_write: bool;
    sector_size: int;
    size_sectors: int64;
  }

  let get_info = function
    | `Nbd t ->
      Nbd_lwt_unix.Client.get_info t
    | `Local t ->
      Block.get_info t

  let read t off bufs = match t with
    | `Nbd t -> Nbd_lwt_unix.Client.read t off bufs
    | `Local t ->
      Block.read t off bufs
      >>= function
      | Result.Error `Disconnected -> Lwt.return_error `Disconnected
      | Result.Error `Unimplemented -> Lwt.return_error `Unimplemented
      | Result.Error _ -> Lwt.return_error `Disconnected
      | Result.Ok x -> Lwt.return_ok x

  let write t off bufs = match t with
    | `Nbd t -> Nbd_lwt_unix.Client.write t off bufs
    | `Local t ->
      Block.write t off bufs
      >>= function
      | Result.Error `Disconnected -> Lwt.return_error `Disconnected
      | Result.Error `Unimplemented -> Lwt.return_error `Unimplemented
      | Result.Error `Is_read_only -> Lwt.return_error `Is_read_only
      | Result.Error _ -> Lwt.return_error `Disconnected
      | Result.Ok x -> Lwt.return_ok x

  let disconnect t = match t with
    | `Nbd t -> Nbd_lwt_unix.Client.disconnect t
    | `Local t -> Block.disconnect t
end

open Cmdliner

(* Help sections common to all commands *)

let _common_options = "COMMON OPTIONS"
let help = [
  `S _common_options;
  `P "These options are common to all commands.";
  `S "MORE HELP";
  `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."; `Noblank;
  `S "BUGS"; `P (Printf.sprintf "Check bug reports at %s" project_url);
]

(* Options common to all commands *)
let common_options_t =
  let docs = _common_options in
  let debug =
    let doc = "Give only debug output." in
    Arg.(value & flag & info ["debug"] ~docs ~doc) in
  let verb =
    let doc = "Give verbose output." in
    let verbose = true, Arg.info ["v"; "verbose"] ~docs ~doc in
    Arg.(last & vflag_all [false] [verbose]) in
  Term.(pure Common.make $ debug $ verb)

module Impl = struct
  open Nbd

  let require name arg = match arg with
    | None -> failwith (Printf.sprintf "Please supply a %s argument" name)
    | Some x -> x

  let require_str name arg =
    require name (if arg = "" then None else Some arg)

  let size host port export =
    let res =
      Nbd_lwt_unix.connect host port
      >>= fun client ->
      Client.negotiate client export in
    let (_,size,_) = Lwt_main.run res in
    Printf.printf "%Ld\n%!" size;
    `Ok ()

  let list _common host port =
    let t =
      Nbd_lwt_unix.connect host port
      >>= fun channel ->
      Client.list channel
      >>= function
      | Result.Ok disks ->
        List.iter print_endline disks;
        Lwt.return ()
      | Result.Error `Unsupported ->
        Printf.fprintf stderr "The server does not support the query function.\n%!";
        exit 1
      | Result.Error `Policy ->
        Printf.fprintf stderr "The server configuration does not permit listing exports.\n%!";
        exit 2 in
    `Ok (Lwt_main.run t)

  let get_exportname = function Some e -> e | None -> ""

  let fail_on_option_error msg p =
    p >>= function
    | Error e -> Lwt.fail_with (msg ^ ": " ^ (Protocol.OptionError.to_string e))
    | Ok x -> Lwt.return x

  let require_structured_reply c =
    Nbd.Client.negotiate_structured_reply c |> fail_on_option_error "Failed to negotiate structured replies"

  let require_export c export =
    Nbd.Client.request_export c export >>= function
    | Ok x -> Lwt.return x
    | Error e -> Lwt.fail_with (Protocol.OptionError.to_string e)

  let list_meta_contexts _common (connection, export) queries =
    let t =
      let export = get_exportname export in
      Nbd_lwt_unix.with_channel_of_connection connection
        (fun channel ->
           Nbd.Client.connect channel >>= fun c ->
           require_structured_reply c >>= fun () ->
           Nbd.Client.list_meta_contexts c export queries |> fail_on_option_error "Failed to list meta contexts" >>= fun l ->
           Lwt_io.printl "Got meta contexts:" >>= fun () ->
           Lwt_list.iter_s Lwt_io.printl l >>= fun () ->
           Nbd.Client.abort c
        )
    in
    Lwt_main.run t

  let query_block_status _common (connection, export) offset length queries =
    let t =
      let export = get_exportname export in
      Nbd_lwt_unix.with_channel_of_connection connection
        (fun channel ->
           Nbd.Client.connect channel >>= fun c ->
           require_structured_reply c >>= fun () ->
           Nbd.Client.set_meta_contexts c export queries |> fail_on_option_error "Failed to set meta contexts" >>= fun l ->
           Lwt_io.printl "Got meta contexts:" >>= fun () ->
           Lwt_list.iter_s (fun (id, name) -> Lwt_io.printlf "%ld: %s" id name) l >>= fun () ->
           require_export c export >>= fun (c, _, _block_sizes) ->
           (* Here we assume that the CLI user's request respects the max block size *)
           Nbd.Client.query_block_status c offset length >>= fun res ->
           begin match res with
             | Error e -> Lwt.fail_with (Fmt.to_to_string Client.pp_error e)
             | Ok l ->
               Lwt_list.iter_s
                 (fun x -> Lwt_io.printl (Protocol.StructuredReplyChunk.BlockStatusChunk.to_string x))
                 l
           end >>= fun () ->
           Nbd.Client.disconnect c
        )
    in
    Lwt_main.run t

  let query_info _common (connection, export) infos =
    let t =
      let export = get_exportname export in
      Nbd_lwt_unix.with_channel_of_connection connection
        (fun channel ->
           Nbd.Client.connect channel >>= fun c ->
           Nbd.Client.query_info c export infos >>= fun res ->
           begin match res with
             | Error e -> Lwt.fail_with (Protocol.OptionError.to_string e)
             | Ok l ->
               Lwt_io.printl "Got infos:" >>= fun () ->
               Lwt_list.iter_s
                 (fun x -> Lwt_io.printl (Protocol.InfoResponse.to_string x))
                 l
           end >>= fun () ->
           Nbd.Client.abort c
        )
    in
    Lwt_main.run t

  let try_negotiate_structured_reply c =
    Nbd.Client.negotiate_structured_reply c >>=
    (function
      | Ok () -> Lwt_io.eprintl "Using structured replies"
      | Error e -> Lwt_io.eprintf "Could not negotiate structured replies: %s" (Protocol.OptionError.to_string e))

  let download _common (connection, export) offset length output =
    let t =
      let export = get_exportname export in
      Nbd_lwt_unix.with_channel_of_connection connection
        (fun channel ->
           Nbd.Client.connect channel >>= fun c ->
           try_negotiate_structured_reply c >>= fun () ->
           Lwt_unix.openfile output Unix.[O_CREAT; O_RDWR; O_EXCL] 0o600 >>= fun fd ->
           (* The file will be prezeroed, and will only use storage space for
            * the non-null data *)
           Lwt_unix.LargeFile.ftruncate fd length >>= fun () ->
           require_export c export >>= fun (c, _disk_info, block_sizes) ->
           let block_sizes =
             match block_sizes with
             | Some b -> b
             | None -> Protocol.default_client_blocksize
           in
           let max_block_size = block_sizes.Protocol.InfoResponse.max in
           let buf = Cstruct.create (max_block_size |> Int32.to_int) in
           let download offset length =
             Nbd.Client.read_chunked c offset length
               (function
                 | `Data (channel, offset, length) ->
                   let rec loop = function
                     | 0 -> Lwt.return_unit
                     | length ->
                       let l = min length (Cstruct.len buf) in
                       let buf = Cstruct.sub buf 0 l in
                       channel.read buf >>= fun () ->
                       Lwt_cstruct.(complete (write fd)) buf >>= fun () ->
                       loop (length - l)
                   in
                   Lwt_unix.LargeFile.lseek fd offset Unix.SEEK_SET >>= fun new_offs ->
                   (if new_offs <> offset then Lwt.fail_with "seek failed" else Lwt.return_unit) >>= fun () ->
                   loop (Int32.to_int length)
                 | `Hole _ -> Lwt.return_unit
               ) >>= function
             | Error e -> Lwt.fail_with (Protocol.Error.to_string e)
             | Ok () -> Lwt.return_unit
           in
           (* We need to download in chunks because the max request length is limited *)
           let rec loop offset length =
             if length <= (Int64.of_int32 max_block_size) then
               download offset (Int64.to_int32 length)
             else
               download offset max_block_size >>= fun () ->
               let length = Int64.sub length (Int64.of_int32 max_block_size) in
               let offset = Int64.add offset (Int64.of_int32 max_block_size) in
               loop offset length
           in
           loop offset length >>= fun () ->
           Lwt_unix.close fd >>= fun () ->
           Nbd.Client.disconnect c
        )
    in
    Lwt_main.run t

  (* Helper function for use within this module *)
  let init_tls_get_server_ctx ~curve ~certfile ~ciphersuites no_tls =
    if no_tls then None
    else (
      let certfile = require_str "certfile" certfile in
      let ciphersuites = require_str "ciphersuites" ciphersuites in
      Some (Nbd_lwt_unix.TlsServer
              (Nbd_lwt_unix.init_tls_get_ctx ~curve ~certfile ~ciphersuites)
           )
    )

  let ignore_exn t () = Lwt.catch t (fun _ -> Lwt.return_unit)

  let serve_common serve_export port exportname certfile curve ciphersuites no_tls =
    let tls_role = init_tls_get_server_ctx ~curve ~certfile ~ciphersuites no_tls in
    let validate ~client_exportname =
      match exportname with
      | Some exportname when exportname <> client_exportname ->
        Lwt.fail_with
          (Printf.sprintf "Client requested invalid exportname %s, name of the export is %s" client_exportname exportname)
      | _ -> Lwt.return_unit
    in
    let handle_connection fd =
      Lwt.finalize
        (fun () ->
           Nbd_lwt_unix.with_channel fd tls_role
             (fun clearchan ->
                let offer = match exportname with
                  | None -> None
                  | Some exportname -> Some [exportname]
                in
                Server.with_connection
                  ?offer
                  clearchan
                  (fun client_exportname svr ->
                     validate ~client_exportname >>= fun () ->
                     serve_export svr client_exportname
                  )
             )
        )
        (ignore_exn (fun () -> Lwt_unix.close fd))
    in
    let t =
      let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Lwt.finalize
        (fun () ->
           Lwt_unix.setsockopt sock Lwt_unix.SO_REUSEADDR true;
           let sockaddr = Lwt_unix.ADDR_INET(Unix.inet_addr_any, port) in
           Lwt_unix.bind sock sockaddr >>= fun () ->
           Lwt_unix.listen sock 5;
           let rec loop () =
             Lwt_unix.accept sock
             >>= fun (fd, _) ->
             (* Background thread per connection *)
             let _ =
               Lwt.catch
                 (fun () -> handle_connection fd)
                 (fun e -> Lwt_log.error_f "Caught exception %s while handling connection" (Printexc.to_string e))
             in
             loop ()
           in
           loop ()
        )
        (ignore_exn (fun () -> Lwt_unix.close sock))
    in
    Lwt_main.run t

  let serve _common filename =
    let filename = require "filename" filename in
    let serve_export svr _exportname =
      Nbd_lwt_unix.with_block filename
        (Server.serve svr ~read_only:false (module Block))
    in
    serve_common serve_export

  let proxy _common (connection, export) =
    let serve_export svr _exportname =
      let export = get_exportname export in
      Nbd_lwt_unix.with_channel_of_connection connection
        (fun channel ->
           Nbd.Client.connect channel >>= fun c ->
           try_negotiate_structured_reply c >>= fun () ->
           Nbd.Client.request_export c export >>=
           (function
             | Ok (c, _disk_info, _block_sizes) ->
               Server.proxy svr ~read_only:false (module Nbd.Client) c >>= fun () ->
               Nbd.Client.disconnect c
             | Error e ->
               Lwt.fail_with ("Failed to connect to export: " ^ (Protocol.OptionError.to_string e)))
           >>= fun () ->
           Nbd.Client.abort c
        )
    in
    serve_common serve_export

  let mirror _common filename port secondary certfile curve ciphersuites no_tls =
    let tls_role = init_tls_get_server_ctx ~curve ~certfile ~ciphersuites no_tls in
    let filename = require "filename" filename in
    let secondary = require "secondary" secondary in
    let t =
      let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let sockaddr = Lwt_unix.ADDR_INET(Unix.inet_addr_any, port) in
      Lwt_unix.bind sock sockaddr >>= fun () ->
      Lwt_unix.listen sock 5;
      let module M = Mirror.Make(Device)(Device) in
      ( Device.connect (Uri.of_string filename)
        >>= fun primary ->
        (* Connect to the secondary *)
        Device.connect (Uri.of_string secondary)
        >>= fun secondary ->
        let progress_cb = function
          | `Complete ->
            Printf.fprintf stderr "Mirror synchronised\n%!"
          | `Percent x ->
            Printf.fprintf stderr "Mirror %d %% complete\n%!" x in
        M.connect ~progress_cb primary secondary
      ) >>= fun m ->
      let rec loop () =
        Lwt_unix.accept sock
        >>= fun (fd, _) ->
        (* Background thread per connection *)
        let _ =
          let channel = Nbd_lwt_unix.cleartext_channel_of_fd fd tls_role in
          Server.connect channel ()
          >>= fun (_name, t) ->
          Server.serve t (module M) m in
        loop () in
      loop () in
    Lwt_main.run t
end

let size_cmd =
  let doc = "Return the size of a disk served over NBD" in
  let host =
    let doc = "Hostname of NBD server" in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"hostname") in
  let port =
    let doc = "Remote port" in
    Arg.(required & pos 1 (some int) None & info [] ~doc ~docv:"port") in
  let export =
    let doc = "Name of the export" in
    Arg.(value & opt string "export" & info [ "export" ] ~doc ~docv:"export") in
  Term.(ret (pure Impl.size $ host $ port $ export)),
  Term.info "size" ~version:"1.0.0" ~doc

(* Used by both server and proxy cmds *)
let exportname =
  let doc = {|Export name to use when serving the file. If specified, clients
              will be able to list this export, and only this export name
              will be accepted. If unspecified, listing the exports will not
              be allowed, and all export names will be accepted when
              connecting.|}
  in
  Arg.(value & opt (some string) None & info ["exportname"] ~doc)

(* Used by both serve and mirror cmds *)
let certfile =
  let doc = "Path to file containing TLS certificate." in
  Arg.(value & opt string "" & info ["certfile"] ~doc)
let ciphersuites =
  let doc = "Set of ciphersuites for TLS (specified in the format accepted by OpenSSL, stunnel etc.)" in
  Arg.(value & opt string "ECDHE-RSA-AES256-GCM-SHA384" & info ["ciphersuites"] ~doc)
let curve =
  let doc = "EC curve to use" in
  Arg.(value & opt string "secp384r1" & info ["curve"] ~doc)
let no_tls =
  let doc = "Use NOTLS mode (refusing TLS) instead of the default FORCEDTLS." in
  Arg.(value & flag & info ["no-tls"] ~doc)
let port =
  let doc = "Local port to listen for connections on" in
  Arg.(value & opt int 10809 & info [ "port" ] ~doc)

let uri_t i =
  let uri =
    let doc = {|NBD uri of the form
                "nbd:unix:<domain-socket>[:exportname=<export>]" or
                "nbd:<server>:<port>[:exportname=<export>]"|}
    in
    Arg.(required & pos i (some string) None & info [] ~doc ~docv:"uri") in
  let parse_uri uri =
    match Nbd.Nbd_uri.parse uri with
    | Ok _ as r -> r
    | Error () -> Error (`Msg ("Invalid NBD URI: " ^ uri))
  in
  Term.(term_result ~usage:true (const parse_uri $ uri))

let serve_cmd =
  let doc = "serve a disk over NBD" in
  let man = [
    `S "DESCRIPTION";
    `P "Create a server which allows a client to access a disk using NBD.";
  ] @ help in
  let filename =
    let doc = "Disk (file or block device) to expose" in
    Arg.(value & pos 0 (some file) None & info [] ~doc) in
  Term.(ret(pure Impl.serve $ common_options_t $ filename $ port $ exportname $ certfile $ curve $ ciphersuites $ no_tls)),
  Term.info "serve" ~sdocs:_common_options ~doc ~man

let proxy_cmd =
  let doc = "proxy an NBD export" in
  let man = [
    `S "DESCRIPTION";
    `P "Create a server which allows a client to access a given NBD export.";
  ] @ help in
  Term.(ret(pure Impl.proxy $ common_options_t $ uri_t 0 $ port $ exportname $ certfile $ ciphersuites $ curve $ no_tls)),
  Term.info "proxy" ~sdocs:_common_options ~doc ~man

let mirror_cmd =
  let doc = "serve a disk over NBD while mirroring" in
  let man = [
    `S "DESCRIPTION";
    `P "Create a server which allows a client to access a disk using NBD.";
    `P "The server will pass I/O through to a primary disk underneath, while also mirroring the contents to a secondary.";
    `S "EXAMPLES";
  ] @ help in
  let filename =
    let doc = "URI naming the primary disk" in
    Arg.(value & pos 0 (some string) None & info [] ~doc) in
  let secondary =
    let doc = "URI naming the secondary disk" in
    Arg.(value & pos 1 (some string) None & info [] ~doc) in
  Term.(ret(pure Impl.mirror $ common_options_t $ filename $ port $ secondary $ certfile $ curve $ ciphersuites $ no_tls)),
  Term.info "mirror" ~sdocs:_common_options ~doc ~man

let list_cmd =
  let doc = "list the disks exported by an NBD server" in
  let man = [
    `S "DESCRIPTION";
    `P "Queries a server and returns a list of known exports. Note older servers may not support the protocol option: this will result in an empty list.";
  ] @ help in
  let host =
    let doc = "Hostname of NBD server" in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"hostname") in
  let port =
    let doc = "Remote port" in
    Arg.(required & pos 1 (some int) None & info [] ~doc ~docv:"port") in
  Term.(ret(pure Impl.list $ common_options_t $ host $ port)),
  Term.info "list" ~sdocs:_common_options ~doc ~man

let queries_t i =
  let doc = "Possibly empty list of meta context queries" in
  Arg.(value & pos_right i string [] & info [] ~doc ~docv:"query")

let list_meta_contexts_cmd =
  let doc = "list the available metadata contexts for an export" in
  let man = [
    `S "DESCRIPTION";
    `P "Queries a server and returns the list NBD metadata contexts available for the given export that match one of the queries. Note: this will fail for older servers that do not support fixed-newstyle negotiation.";
  ] @ help in
  let uri = uri_t 0 in
  let queries = queries_t 0 in
  Term.(pure Impl.list_meta_contexts $ common_options_t $ uri $ queries),
  Term.info "list-meta-contexts" ~sdocs:_common_options ~doc ~man

let query_block_status_cmd =
  let doc = "query the block status for the selected meta contexts in the given range" in
  let man = [
    `S "DESCRIPTION";
    `P "Queries a server and returns the block status descriptors for the given range, for each NBD metadata context available for the given export that match one of the queries. Note: this will fail for older servers that do not support fixed-newstyle negotiation.";
  ] @ help in
  let uri = uri_t 0 in
  let offset =
    let doc = "The start of the range from which block status descriptors should be returned" in
    Arg.(required & pos 1 (some int64) None & info [] ~doc ~docv:"offset")
  in
  let length =
    let doc = "The length of the range from which block status descriptors should be returned" in
    Arg.(required & pos 2 (some int32) None & info [] ~doc ~docv:"length")
  in
  let queries = queries_t 2 in
  Term.(pure Impl.query_block_status $ common_options_t $ uri $ offset $ length $ queries),
  Term.info "query-block-status" ~sdocs:_common_options ~doc ~man

let query_info_cmd =
  let doc = "query information about an export" in
  let man = [
    `S "DESCRIPTION";
    `P "Get details about the export. This may not be supported by the server.";
  ] @ help in
  let uri = uri_t 0 in
  let infos =
    let info_conv =
      let parse s =
        match int_of_string s |> Nbd.Protocol.Info.of_int with
        | Unknown n -> Error (`Msg ("Unknown info type: " ^ (string_of_int n)))
        | n -> Ok n
        | exception e -> Error (`Msg (Printexc.to_string e))
      in
      let print fmt s = Format.pp_print_string fmt (Nbd.Protocol.Info.to_string s) in
      Arg.conv (parse, print)
    in
    let doc = "Possibly empty list of information request numbers, as defined by the NBD protocol (NBD_INFO)" in
    Arg.(value & pos_right 0 info_conv [] & info [] ~doc ~docv:"infos")
  in
  Term.(pure Impl.query_info $ common_options_t $ uri $ infos),
  Term.info "query-info" ~sdocs:_common_options ~doc ~man

let download_cmd =
  let doc = "download the whole or part of an export" in
  let man = [
    `S "DESCRIPTION";
    `P "Downloads the specified region of the NBD export, using structured replies for skipping holes in the export and only writing the non-null data to a sparse file, if possible.";
  ] @ help in
  let uri = uri_t 0 in
  let offset =
    let doc = "The start of the region to download" in
    Arg.(required & pos 1 (some int64) None & info [] ~doc ~docv:"offset")
  in
  let length =
    let doc = "The length of the region to download" in
    Arg.(required & pos 2 (some int64) None & info [] ~doc ~docv:"length")
  in
  let output =
    let doc = "The output file" in
    Arg.(required & pos 3 (some string) None & info [] ~doc ~docv:"output")
  in
  Term.(pure Impl.download $ common_options_t $ uri $ offset $ length $ output),
  Term.info "download" ~sdocs:_common_options ~doc ~man

let default_cmd =
  let doc = "manipulate NBD clients and servers" in
  let man = help in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_options_t)),
  Term.info "nbd-tool" ~version:"1.0.0" ~sdocs:_common_options ~doc ~man

let cmds = [serve_cmd; proxy_cmd; list_cmd; size_cmd; list_meta_contexts_cmd; query_block_status_cmd; query_info_cmd; download_cmd; mirror_cmd]

let () =
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0
