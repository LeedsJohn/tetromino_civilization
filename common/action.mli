(** Stuff that can be done to a [Board.t]. *)
open! Core

type t =
  | Player_move of Client_id.t * Player_move.t
  | Spawn_piece of Client_id.t * Coordinate.t * Piece_type.t
  | Delete_row of int
  | Delete_chunk of int
  | Add_chunk
  | Fill_tile of Coordinate.t
[@@deriving sexp]
