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

(* Store each element of an extent separately in a Set rather than
   anything more clever *)

module Int64Set = Set.Make(Int64)

(* Round access to the nearest 'block' to avoid storing too much detail. *)
let block_size = 1024 * 64

type t = Int64Set.t

let empty = Int64Set.empty

type start = int64

type length = int

let rec add ~start ~length t = match length with
  | 0 -> t
  | _ -> add ~start:(Int64.succ start) ~length:(length - 1) (Int64Set.add start t)

let fold f t init = Int64Set.fold (fun elt acc -> f elt 1 acc) t init

let cardinal = Int64Set.cardinal
