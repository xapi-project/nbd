(* Returns the size of disk served over NBD *)

open Lwt
open Cmdliner

let size host port =
  let res =  
    Nbd_lwt_client.open_channel host port >>= 
    Nbd_lwt_client.negotiate in
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
  Term.(ret (pure size $ host $ port)),
  Term.info "nbd-size" ~version:"1.0.0" ~doc

let _ =
  match Term.eval default_cmd with 
  | `Error _ -> exit 1
  | _ -> exit 0