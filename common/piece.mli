(** Represents a tetromino. *)
open! Core

type t [@@deriving equal, sexp_of]

(** Container functions over the coordinates of the piece. *)
include Container.S0 with type t := t and type elt := Coordinate.t

val make
  :  ?rotate_count:int
  -> piece_type:Piece_type.t
  -> pivot_position:Coordinate.t
  -> unit
  -> t

(** Every piece has a designated pivot position that it is rotated around. It can be
    thought of as a single [Coordinate.t] representing where the piece is. *)
val pivot_position : t -> Coordinate.t

val piece_type : t -> Piece_type.t
val coordinates : t -> Coordinate.t list
val rotate : t -> dir:[ `Clockwise | `Counter_clockwise ] -> t
val move : t -> dir:Coordinate.t -> t
