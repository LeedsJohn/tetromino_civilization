open! Core

type t = int * int [@@deriving bin_io, compare, equal, sexp]

let row_col (row, col) = row, col
let row (row, _col) = row
let col (_row, col) = col
let make ~row ~col = row, col
let add (row1, col1) (row2, col2) = row1 + row2, col1 + col2
let down = make ~row:(-1) ~col:0
let up = make ~row:1 ~col:0
let left = make ~row:0 ~col:(-1)
let right = make ~row:0 ~col:1
let origin = make ~row:0 ~col:0
