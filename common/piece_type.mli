open! Core

type t =
  | I
  | O
  | T
  | S
  | Z
  | J
  | L
[@@deriving equal, sexp_of]

val to_char : t -> char
