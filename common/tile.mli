open! Core

type t =
  | Empty
  | Full
[@@deriving equal, sexp_of]

val to_char : t -> char
