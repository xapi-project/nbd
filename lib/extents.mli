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

type t
(** A set of extents *)

val empty: t
(** An empty set *)

type start = int64
(** The start of an extent *)

type length = int
(** The length of an extent *)

val add: start:start -> length:length -> t -> t
(** [add start length t] returns a set with the same contents as [t]
    plus the range [start-length]. *)

val fold: (start -> length -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold f t init] folds [f] across all the extents in [t] in order
    of increasing [start]. *)

val cardinal: t -> int
(** [cardinal t] returns the total size of the extents within [t] *)
