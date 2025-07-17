open! Core
(* open Tetromino_civilization_common *)

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
  (* let _incr_row row = (row + 1) % num_rows t in
   let rec col_loop row = 
        if 5 % 3 *)
  ()
;;

let get t _ = t.data.(0).(0)
let set _ _ _ = ()

let%expect_test "bruh" =
  print_s [%sexp (-1 % 3 : int)];
  [%expect {| |}]
;;
