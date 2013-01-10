val negotiate : Unix.file_descr -> int64 * Nbd.flag list
val read : Unix.file_descr -> int64 -> int32 -> string option
val write : Unix.file_descr -> int64 -> string -> int -> int -> int32 option
val write_async : Unix.file_descr -> int64 -> string -> int -> int -> int64 -> unit
val write_wait : Unix.file_descr -> int64 * int32 option
val disconnect_async : Unix.file_descr -> int64 -> unit
val connect : string -> int -> Unix.file_descr * int64 * Nbd.flag list
