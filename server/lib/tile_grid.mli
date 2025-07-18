open! Core
open Tetromino_civilization_common

type t

val make : rows:int -> cols:int -> t

(** Deletes row at given index. Shifts every row above that down by one and adds a new row
    to the end. *)
val delete_row : t -> int -> unit

val get : t -> Coordinate.t -> Tile.t
val set : t -> Coordinate.t -> Tile.t -> unit
val num_rows : t -> int
val num_cols : t -> int
