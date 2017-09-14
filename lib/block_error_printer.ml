
(** Converts [Block.error] errors to string for debug printing. *)
(* This function will become unnecessary once we move to newer Mirage block
   libraries that define Block.pp_write_error for printing errors. *)
let to_string = function
  | `Disconnected -> "Disconnected: device is disconnected"
  | `Is_read_only -> "Is_read_only: attempted to write to a read-only disk"
  | `Unimplemented -> "Unimplemented: operation not yet implemented"
  | `Unknown s -> "Unknown: " ^ s
