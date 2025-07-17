open! Core
open Tetromino_civilization_common

type 'a t

val make : rows:int -> cols:int -> default:'a -> 'a t

(** Deletes row at given index. Shifts every row above that down by one and adds a new row
    to the end. *)
val delete_row : 'a t -> int -> unit

val get : 'a t -> Coordinate.t -> 'a
val set : 'a t -> Coordinate.t -> 'a -> unit
