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

  let serve common filename port =
    let filename = require "filename" filename in
    let port = require "port" port in
    let stats = Unix.LargeFile.stat filename in
    let size = stats.Unix.LargeFile.st_size in
    let flags = [] in
    let t =
      let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 1 in
      let sockaddr = Lwt_unix.ADDR_INET(Unix.inet_addr_any, port) in
      Lwt_unix.bind sock sockaddr;
      Lwt_unix.listen sock 5;
      while_lwt true do
        lwt (fd, _) = Lwt_unix.accept sock in
        (* Background thread per connection *)
        let _ =
          lwt server = Nbd_lwt_server.negotiate fd size flags in
          try_lwt
            while_lwt true do
              lwt request = Nbd_lwt_server.next server in
              match request.Request.ty with
              | Command.Read 
              | Command.Write
              | _ ->
                Nbd_lwt_server.error server request.Request.handle 1l
            done
          with e ->
            Printf.fprintf stderr "Caught %s; closing connection\n%!" (Printexc.to_string e);
            return () in
        return ()
      done in
    Lwt_main.run t;
    `Ok ()

end

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
    Arg.(value & pos 1 (some int) None & info [] ~doc) in
  Term.(ret(pure Impl.serve $ common_options_t $ filename $ port)),
  Term.info "serve" ~sdocs:_common_options ~doc ~man

let default_cmd = 
  let doc = "manipulate NBD clients and servers" in
  let man = help in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_options_t)),
  Term.info "nbd-tool" ~version:"1.0.0" ~sdocs:_common_options ~doc ~man
       
let cmds = [serve_cmd]

let _ =
  match Term.eval_choice default_cmd cmds with 
  | `Error _ -> exit 1
  | _ -> exit 0
