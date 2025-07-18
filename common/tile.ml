open! Core

type t =
  | Empty
  | Full
[@@deriving equal, sexp_of]

let to_char = function
  | Empty -> '.'
  | Full -> '#'
;;
