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

val to_char : t -> char
val is_empty : t -> bool
