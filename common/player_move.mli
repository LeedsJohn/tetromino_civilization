(** Moves that a player can make. *)
open! Core

type t =
  | Right
  | Left
  | Down
  | Drop
  | Clockwise
  | Counter_clockwise
[@@deriving sexp]
