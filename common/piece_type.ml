open! Core

type t =
  | I
  | O
  | T
  | S
  | Z
  | J
  | L
[@@deriving compare, equal, sexp]

let to_char = function
  | I -> 'I'
  | O -> 'O'
  | T -> 'T'
  | S -> 'S'
  | Z -> 'Z'
  | J -> 'J'
  | L -> 'L'
;;
