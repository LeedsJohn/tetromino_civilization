open! Core

type t = int * int

let row_col (row, col) = row, col
let make ~row ~col = row, col
