(*
 * Copyright (C) 2011-2013 Citrix Inc
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

open Common
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
  open Lwt
  open Nbd

  let require name arg = match arg with
    | None -> failwith (Printf.sprintf "Please supply a %s argument" name)
    | Some x -> x

  let size host port export =
    let res =
      Nbd_lwt_channel.connect host port
      >>= fun client ->
      Nbd_lwt_client.negotiate client export in
    let (_,size,_) = Lwt_main.run res in
    Printf.printf "%Ld\n%!" size;
    `Ok ()

  let list common host port =
    let t =
      Nbd_lwt_channel.connect host port
      >>= fun channel ->
      Nbd_lwt_client.list channel
      >>= function
      | `Ok disks ->
        List.iter print_endline disks;
        return ()
      | `Error `Unsupported ->
        Printf.fprintf stderr "The server does not support the query function.\n%!";
        exit 1
      | `Error `Policy ->
        Printf.fprintf stderr "The server configuration does not permit listing exports.\n%!";
        exit 2 in
    `Ok (Lwt_main.run t)

  let serve common filename port =
    let filename = require "filename" filename in
    let stats = Unix.LargeFile.stat filename in
    let size = stats.Unix.LargeFile.st_size in
    let flags = [] in
    let t =
      let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let sockaddr = Lwt_unix.ADDR_INET(Unix.inet_addr_any, port) in
      Lwt_unix.bind sock sockaddr;
      Lwt_unix.listen sock 5;
      while_lwt true do
        lwt (fd, _) = Lwt_unix.accept sock in
        (* Background thread per connection *)
        let _ =
          let channel = Nbd_lwt_channel.of_fd fd in
          Nbd_lwt_server.negotiate_begin channel ()
          >>= fun (name, t) ->
          Nbd_lwt_server.negotiate_end t size flags
          >>= fun server ->
          try_lwt
            let block_size = 32768 in
            let block = Cstruct.create block_size in
            while_lwt true do
              lwt request = Nbd_lwt_server.next server in
              Printf.fprintf stderr "%s\n%!" (Request.to_string request);
              match request.Request.ty with
              | Command.Write ->
                let rec loop remaining =
                  let n = min block_size remaining in
                  let subblock = Cstruct.sub block 0 n in
                  channel.Nbd_lwt_channel.read subblock
                  >>= fun () ->
                  let remaining = remaining - n in
                  if remaining > 0 then loop remaining else return () in
                lwt () = loop (Int32.to_int request.Request.len) in
                Nbd_lwt_server.ok server request.Request.handle None
              | Command.Read 
              | _ ->
                Nbd_lwt_server.error server request.Request.handle `EINVAL
            done
          with e ->
            Printf.fprintf stderr "Caught %s; closing connection\n%!" (Printexc.to_string e);
            lwt () = Nbd_lwt_server.close server in
            return () in
        return ()
      done in
    Lwt_main.run t;
    `Ok ()

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

let serve_cmd =
  let doc = "serve a disk over NBD" in
  let man = [
    `S "DESCRIPTION";
    `P "Create a server which allows a client to access a disk using NBD.";
  ] @ help in
  let filename =
    let doc = "Disk (file or block device) to expose" in
    Arg.(value & pos 0 (some file) None & info [] ~doc) in
  let port =
    let doc = "Local port to listen for connections on" in
    Arg.(value & opt int 10809 & info [ "port" ] ~doc) in
  Term.(ret(pure Impl.serve $ common_options_t $ filename $ port)),
  Term.info "serve" ~sdocs:_common_options ~doc ~man

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

let default_cmd = 
  let doc = "manipulate NBD clients and servers" in
  let man = help in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_options_t)),
  Term.info "nbd-tool" ~version:"1.0.0" ~sdocs:_common_options ~doc ~man
       
let cmds = [serve_cmd; list_cmd; size_cmd]

let _ =
  match Term.eval_choice default_cmd cmds with 
  | `Error _ -> exit 1
  | _ -> exit 0
