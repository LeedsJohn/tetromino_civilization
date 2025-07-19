open! Core

type t =
  | Empty
  | I
  | O
  | T
  | S
  | Z
  | J
  | L
[@@deriving equal, sexp_of]

let to_char = function
  | Empty -> '.'
  | I -> 'I'
  | O -> 'O'
  | T -> 'T'
  | S -> 'S'
  | Z -> 'Z'
  | J -> 'J'
  | L -> 'L'
;;

let is_empty = function
  | Empty -> true
  | I | O | T | S | Z | J | L -> false
;;
