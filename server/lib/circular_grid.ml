open! Core
(* open Tetromino_civilization_common *)

module Column = struct
    type 'a t = {data : 'a array; mutable start : int; mutable heighest_filled: int}

    let make ~len ~default = {data = Array.create ~len default; start = 0; heighest_filled = (-1)}

    let transform_i t i = (i + t.start) % Array.length t.data

    let delete ({data; start; heighest_filled} as t) i =
        if i <= heighest_filled - i then shuffle_down t i else shuffle_up t i
end

type 'a t =
  { mutable start : int
  ; data : 'a array array
  }

(* TODO: make sure this is laid out so that columns are adjacent in memory. *)
let make ~rows ~cols ~default =
  { start = 0; data = Array.make_matrix ~dimx:cols ~dimy:(rows * 2) default }
;;

let _num_rows t = Array.length t.data.(0)
let _num_cols t = Array.length t.data



let delete_row _t _row =
   let _incr_row row = (row + 1) % num_rows t in
   let rec col_loop row = 
        if 5 % 3 
  ()
;;

let get t _ = t.data.(0).(0)
let set _ _ _ = ()

let%expect_test "bruh" =
  print_s [%sexp (-1 % 3 : int)];
  [%expect {| |}]
;;
