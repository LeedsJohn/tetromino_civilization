open! Core

type t [@@deriving bin_io]

val create
  :  start_num_chunks:int
  -> max_num_chunks:int
  -> num_rows:int
  -> chunk_cols:int
  -> t

val apply_action : t -> Action.t -> bool

(** Prints string representation of the [t]. Loses some information - for example, if two
    players both have a falling piece at the same location, you will not be able to tell
    this from this representation. *)
val show : t -> unit

(** List of coordinates that aren't empty and a character representation of the data at
    that coordinate. Uppercase characters are locked tiles, lowercase characters are
    falling tiles, # is filled tiles.

    Sorted by the [Coordinate.t] followed by the [Char.t] if the [Coordinate.t]s are
    equal. *)
val sorted_coordinate_list : t -> (Coordinate.t * Char.t) list

(** Returns a fresh board. *)
val copy : t -> t

val num_cols : t -> int
val chunks_that_piece_is_inside : t -> Client_id.t -> int list
val cols_per_chunk : t -> int
val num_chunks : t -> int
val get_piece_exn : t -> Client_id.t -> Piece.t
val get_piece : t -> Client_id.t -> Piece.t option
val get_full_rows : t -> int list
