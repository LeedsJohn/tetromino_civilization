(** Create a test by playing a game that can later be replayed.

    [butchered by ocamlformat :( ] Controls: * a -> left * e -> right * ' -> rotate
    counterclockwise * . -> rotate clockwise * o -> down * [space] -> drop * N\d -> Spawn
    a new piece at col \d * d\d -> delete row at \d * (\d,\d) -> Fill the tile at (x, y)
    coordinate (\d, \d) *)

open! Core
open! Tetromino_civilization_common

let char_to_player_move c client_id =
  match c with
  | 'a' -> Action.Player_move (client_id, Player_move.Left) |> Some
  | 'o' -> Action.Player_move (client_id, Player_move.Down) |> Some
  | 'e' -> Action.Player_move (client_id, Player_move.Right) |> Some
  | ' ' -> Action.Player_move (client_id, Player_move.Drop) |> Some
  | '\'' -> Action.Player_move (client_id, Player_move.Counter_clockwise) |> Some
  | '.' -> Action.Player_move (client_id, Player_move.Clockwise) |> Some
  | _ -> None
;;

let string_to_action s client_id =
  let open Option.Let_syntax in
  let%bind c = if String.length s = 0 then None else Some (String.get s 0) in
  if Char.is_digit c && String.length s = 2
  then (
    let%bind action = char_to_player_move (String.get s 1) client_id in
    Some (List.create ~len:(Char.get_digit_exn c) action))
  else (
    match c with
    | 'd' ->
      let%bind row_num = String.drop_prefix s 1 |> Int.of_string_opt in
      Some [ Action.Delete_row row_num ]
    | '(' ->
      let%bind comma_pos = String.index s ',' in
      let%bind x = String.sub s ~pos:1 ~len:(comma_pos - 1) |> Int.of_string_opt in
      let%bind y =
        let s = String.drop_prefix s (comma_pos + 1) in
        String.drop_suffix s 1 |> Int.of_string_opt
      in
      Some [ Action.Fill_tile (Coordinate.make ~row:y ~col:x) ]
    | 'N' ->
      let col = String.drop_prefix s 1 |> Int.of_string_opt |> Option.value ~default:5 in
      let piece_type = List.random_element_exn [ Piece_type.I; O; T; S; Z; J; L ] in
      Some [ Action.Spawn_piece (client_id, col, piece_type) ]
    | _ ->
      let%bind action = char_to_player_move c client_id in
      Some [ action ])
;;

let rec step board client_id res =
  let s = In_channel.input_line In_channel.stdin |> Option.value_exn in
  if String.equal s "quit"
  then List.rev res
  else (
    match string_to_action s client_id with
    | None -> step board client_id res
    | Some actions ->
      let res =
        List.fold actions ~init:res ~f:(fun acc action ->
          let _ = Board.apply_action board action in
          (action, Board.sorted_coordinate_list board) :: acc)
      in
      Board.show board;
      step board client_id res)
;;

let () =
  let fname = (Sys.get_argv ()).(1) |> String.chop_suffix_if_exists ~suffix:".sexp" in
  let fname = [%string "test/assets/%{fname}.sexp"] in
  let board =
    Board.create ~start_num_chunks:1 ~max_num_chunks:1 ~num_rows:15 ~chunk_cols:10
  in
  let res = step board (Client_id.create ()) [] in
  Sexp.save_mach fname [%sexp (res : (Action.t * (Coordinate.t * Char.t) list) list)]
;;
