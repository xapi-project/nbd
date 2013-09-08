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

let really_read sock buf' =
  let ofs = buf'.Cstruct.off in
  let len = buf'.Cstruct.len in
  let buf = buf'.Cstruct.buffer in

  let rec loop acc sock buf ofs len = 
    lwt n = Lwt_bytes.read sock buf ofs len in
    let len = len - n in
    let acc = acc + n in
    if len = 0 || n = 0
    then return acc
    else loop acc sock buf (ofs + n) len in
  lwt actual = loop 0 sock buf ofs len in
  if actual <> len 
  then fail (Failure (Printf.sprintf "Short read; expected %d, got %d" len actual))
  else return ()

let really_write sock buf =
  let ofs = buf.Cstruct.off in
  let len = buf.Cstruct.len in
  let buf = buf.Cstruct.buffer in
  let rec rwrite fd buf ofs len =
    lwt n = Lwt_bytes.write fd buf ofs len in
    if n = 0 then fail End_of_file
    else if n < len then rwrite fd buf (ofs + n) (len - n) else return () in
  rwrite sock buf ofs len
