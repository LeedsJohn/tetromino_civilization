open! Core

type t =
  | Empty
  | Locked of Piece_type.t
  | Filled
[@@deriving bin_io, compare, equal, sexp]

let to_char = function
  | Empty -> '.'
  | Locked piece_type -> Piece_type.to_char piece_type |> Char.uppercase
  | Filled -> '#'
;;

let is_empty = function
  | Empty -> true
  | Locked _ | Filled -> false
;;

let is_locked = function
  | Locked _ -> true
  | Empty | Filled -> false
;;

let is_filled = function
  | Filled -> true
  | Locked _ | Empty -> false
;;

let empty = Empty
let locked piece_type = Locked piece_type
let filled = Filled
