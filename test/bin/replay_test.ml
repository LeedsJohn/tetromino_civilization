(** Step through a test to see where it went wrong. *)

open! Core
open! Tetromino_civilization_common

let get_data fname =
  Sexp.load_sexp_conv_exn fname [%of_sexp: (Action.t * (Coordinate.t * Char.t) list) list]
;;

let show_board coords =
  let max_row, max_col =
    List.fold coords ~init:(0, 0) ~f:(fun (max_row, max_col) (coord, _tile) ->
      let row, col = Coordinate.row_col coord in
      Int.max max_row row, Int.max max_col col)
  in
  let get_char coord =
    List.Assoc.find coords ~equal:Coordinate.equal coord |> Option.value ~default:'.'
  in
  let print_row row =
    List.range 0 (max_col + 1)
    |> List.map ~f:(fun col -> get_char (Coordinate.make ~row ~col))
    |> String.of_list
    |> print_endline
  in
  List.range ~stride:(-1) max_row (-1) |> List.iter ~f:print_row
;;

let () =
  let fname = [%string "test/assets/%{(Sys.get_argv ()).(1)}.sexp"] in
  let data = get_data fname in
  let board =
    Board.create ~start_num_chunks:1 ~max_num_chunks:1 ~num_rows:15 ~chunk_cols:10
  in
  List.iteri data ~f:(fun i (action, coords) ->
    print_s [%message "" (i : int) (action : Action.t)];
    print_endline "expected";
    show_board coords;
    let _ = Board.apply_action board action in
    print_endline "got";
    Board.sorted_coordinate_list board |> show_board;
    print_endline "------------------")
;;
