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

let really_read sock buf n =
  let rec loop acc sock buf ofs len = 
    lwt n = Lwt_bytes.read sock buf ofs len in
    let len = len - n in
    let acc = acc + n in
    if len = 0 || n = 0
    then return acc
    else loop acc sock buf (ofs + n) len in
  lwt actual = loop 0 sock buf 0 n in
  if actual <> n
  then fail (Failure (Printf.sprintf "Short read; expected %d, got %d" n actual))
  else return ()

let really_write sock buf expected =
  lwt actual = Lwt_bytes.write sock buf 0 expected in
  if expected <> actual
  then fail (Failure (Printf.sprintf "Short write; expected %d got %d" expected actual))
  else return ()

