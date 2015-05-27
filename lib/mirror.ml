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
open Lwt

module Make(Primary: V1_LWT.BLOCK)(Secondary: V1_LWT.BLOCK) = struct
  type 'a io = 'a Lwt.t

  type page_aligned_buffer = Cstruct.t

  type error = [
    | `Unknown of string
    | `Unimplemented
    | `Is_read_only
    | `Disconnected
  ]

  let string_of_error = function
    | `Unknown x -> x
    | `Unimplemented -> "Unimplemented"
    | `Is_read_only -> "Device is read-only"
    | `Disconnected -> "Device has been disconnected"

  type info = {
    read_write: bool;
    sector_size: int;
    size_sectors: int64;
  }

  type t = {
    primary: Primary.t;
    secondary: Secondary.t;
    primary_block_size: int; (* number of primary sectors per info.sector_size *)
    secondary_block_size: int; (* number of secondary sectors per info.sector_size *)
    info: info;
    mutable disconnected: bool;
  }

  type id = unit

  let get_info t = return t.info

  let connect primary secondary =
    Primary.get_info primary
    >>= fun primary_info ->
    Secondary.get_info secondary
    >>= fun secondary_info ->

    let sector_size = max primary_info.Primary.sector_size secondary_info.Secondary.sector_size in
    (* We need our chosen sector_size to be an integer multiple of
       both primary and secondary sector sizes. This should be the
       very common case e.g. 4096 and 512; 512 and 1 *)
    let primary_block_size = sector_size / primary_info.Primary.sector_size in
    let secondary_block_size = sector_size / secondary_info.Secondary.sector_size in
    let primary_bytes = Int64.(mul primary_info.Primary.size_sectors (of_int primary_info.Primary.sector_size)) in
    let secondary_bytes = Int64.(mul secondary_info.Secondary.size_sectors (of_int secondary_info.Secondary.sector_size)) in

    ( let open Result in
      ( if sector_size mod primary_info.Primary.sector_size <> 0
        || sector_size mod secondary_info.Secondary.sector_size <> 0
        then fail (`Unknown (Printf.sprintf "Incompatible sector sizes: either primary (%d) or secondary (%d) must be an integer multiple of the other" primary_info.Primary.sector_size secondary_info.Secondary.sector_size))
        else return ()
      ) >>= fun () ->
      ( if primary_bytes <> secondary_bytes
        then fail (`Unknown (Printf.sprintf "Incompatible overall sizes: primary (%Ld bytes) and secondary (%Ld bytes) must be the same size" primary_bytes secondary_bytes))
        else return ()
      ) >>= fun () ->
      ( if not secondary_info.Secondary.read_write
        then fail (`Unknown "Cannot mirror to a read-only secondary device")
        else return ()
      )
    ) |> Lwt.return
    >>= function
    | `Error e -> return (`Error e)
    | `Ok () ->
      let disconnected = false in
      let read_write = primary_info.Primary.read_write in
      let size_sectors = Int64.(div primary_bytes (of_int sector_size)) in
      let info = { read_write; sector_size; size_sectors } in
      return (`Ok { primary; secondary; primary_block_size; secondary_block_size; info; disconnected })

  let read t ofs bufs =
    Primary.read t.primary ofs bufs

  let write t ofs bufs = return (`Error `Unimplemented)

  let disconnect t =
    t.disconnected <- true;
    return ()
end

