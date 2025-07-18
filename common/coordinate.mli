open! Core

type t

val make : row:int -> col:int -> t
val row_col : t -> int * int
