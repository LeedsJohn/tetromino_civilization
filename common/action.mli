(** Stuff that can be done to a [Board.t]. *)
open! Core

type t =
  | Player_move of Client_id.t * Player_move.t
  | Spawn_piece of Client_id.t * int * Piece_type.t (** column to spawn in *)
  | Remove_piece of Client_id.t
  | Set_player_piece of Client_id.t * Piece.t
  | Set_locked_piece of Piece.t
  | Delete_row of int
  | Delete_chunk of int
  | Add_chunk
  | Fill_tile of Coordinate.t
[@@deriving bin_io, equal, sexp]
