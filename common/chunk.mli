(** Slice of a [Board.t]. It has the same number of rows as the [Board.t], but fewer
    columns (assuming the [Board.t] consists of multiple chunks.

    Positions that are locked are stored separately from falling pieces. *)
open! Core

type t [@@deriving equal, sexp_of]

val make : rows:int -> cols:int -> t

(** Deletes row at given index. Shifts every row above that down by one and adds a new row
    to the end. *)
val delete_row : t -> int -> unit

val get : t -> Coordinate.t -> Tile.t
val set : t -> Coordinate.t -> Tile.t -> unit
val get_piece : t -> Client_id.t -> Piece.t
val set_piece : t -> Client_id.t -> Piece.t -> unit
val remove_piece : t -> Client_id.t -> unit

(** Sets all tiles to [Tile.empty] and removes all players. *)
val reset : t -> unit

val num_rows : t -> int
val num_cols : t -> int

(** [with_labels] is true by default. It adds row and column markers to the
    representation. *)
val show : ?with_labels:bool -> t -> unit

val copy : t -> t
