(* Returns the size of disk served over NBD *)

open Lwt
open Cmdliner

let size host port export =
  let res =  
    Nbd_lwt_channel.connect host port
    >>= fun client ->
    Nbd_lwt_client.negotiate client export in
  let (_,size,_) = Lwt_main.run res in
  Printf.printf "%Ld\n%!" size;
  `Ok ()

let default_cmd = 
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
  Term.(ret (pure size $ host $ port $ export)),
  Term.info "nbd-size" ~version:"1.0.0" ~doc

let _ =
  match Term.eval default_cmd with 
  | `Error _ -> exit 1
  | _ -> exit 0
