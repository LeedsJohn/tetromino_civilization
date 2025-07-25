(** Replays games created with bin/make_test.ml and make sure that the boards match up. *)

open! Core
open! Core_unix
open! Tetromino_civilization_common

let get_test_data () =
  Sys_unix.ls_dir "assets/"
  |> List.map ~f:(fun fname ->
    let fname = Filename.concat "assets" fname in
    let actions, boards =
      Sexp.load_sexp_conv_exn
        fname
        [%of_sexp: (Action.t * (Coordinate.t * Char.t) list) list]
      |> List.unzip
    in
    fname, actions, boards)
;;

let run_test test_name actions correct_boards =
  let board =
    Board.create ~start_num_chunks:1 ~max_num_chunks:1 ~num_rows:15 ~chunk_cols:10
  in
  let boards =
    List.map actions ~f:(fun action ->
      let _ = Board.apply_action board action in
      Board.sorted_coordinate_list board)
  in
  let res =
    List.zip_exn correct_boards boards
    |> List.findi ~f:(fun _i (correct_board, board) ->
      not ([%equal: (Coordinate.t * Char.t) list] correct_board board))
  in
  match res with
  | None -> ()
  | Some (action_number, _) ->
    print_s [%message "Failed test" (test_name : string) (action_number : int)]
;;

let%expect_test "board manipulation" =
  get_test_data ()
  |> List.iter ~f:(fun (test_name, actions, correct_boards) ->
    run_test test_name actions correct_boards);
  [%expect {| |}]
;;
