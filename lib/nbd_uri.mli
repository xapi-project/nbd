
type connection =
  | Tcp of string * int
  | UnixDomainSocket of string

val parse : string -> (connection * string option, unit) result
(** Extracts the UNIX domain socket path and the export name from the NBD URI
    format that qemu uses. *)
