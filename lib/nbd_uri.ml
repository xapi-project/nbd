
type connection =
  | Tcp of string * int
  | UnixDomainSocket of string

let parse uri =
  let fail () = Error () in
  let parse_exportname exportname =
    let prefix = "exportname=" in
    if String.length exportname < String.length prefix ||
       String.sub exportname 0 (String.length prefix) <> prefix then fail ()
    else
      let exportname = String.sub exportname (String.length prefix) (String.length exportname - String.length prefix) in
      Ok exportname
  in
  match String.split_on_char ':' uri with
  | ["nbd"; "unix"; socket] ->
    Ok (UnixDomainSocket socket, None)
  | ["nbd"; "unix"; socket; exportname] -> begin
      match parse_exportname exportname with
      | Ok exportname -> Ok (UnixDomainSocket socket, Some exportname)
      | Error _ -> fail ()
    end
  | ["nbd"; ip; port] -> begin
      match int_of_string port with
      | port -> Ok (Tcp (ip, port), None)
      | exception _ -> fail ()
    end
  | ["nbd"; ip; port; exportname] -> begin
      match int_of_string port, parse_exportname exportname with
      | port, Ok exportname -> Ok (Tcp (ip, port), Some exportname)
      | _, Error _ -> fail ()
      | exception _ -> fail ()
    end
  | _ -> fail ()
