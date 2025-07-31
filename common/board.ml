open! Core

module Row_filled_cells_counter = struct
  module Ar = Circular_array.Make (struct
      type t = int [@@deriving bin_io, sexp_of]
    end)

  type t = Ar.t [@@deriving bin_io, sexp_of]

  let make ~len =
    let t = Ar.make ~max_len:len ~start_len:len in
    List.range 0 len |> List.iter ~f:(fun i -> Ar.set_some t i 0);
    t
  ;;

  let copy = Ar.copy
  let incr t i = Ar.set_some t i (Ar.get_some_exn t i + 1)
  let decr t i = Ar.set_some t i (Ar.get_some_exn t i - 1)

  let delete_and_add_row t i =
    Ar.delete t i;
    Ar.append_some t 0
  ;;

  let to_array = Ar.to_some_array
  let get t i = Ar.get_some_exn t i
end

module Int_hash_set = Hash_set.Make_binable (Int)

(* A player can potentially be in two chunks simultaneously if their piece spans two
   chunks.
   The second int is set to (-1) if the player is only in one chunk. *)
type t =
  { data : Chunk_array.t
  ; player_chunk_ids : int list Hashtbl.M(Client_id).t
  ; row_filled_cells : Row_filled_cells_counter.t
  ; full_rows : Int_hash_set.t
  }
[@@deriving bin_io]

let copy t =
  { data = Chunk_array.copy t.data
  ; player_chunk_ids = Hashtbl.copy t.player_chunk_ids
  ; row_filled_cells = Row_filled_cells_counter.copy t.row_filled_cells
  ; full_rows = Hash_set.copy t.full_rows
  }
;;

let num_chunks t = Chunk_array.length t.data

let add_chunk t =
  if Chunk_array.length t.data < Chunk_array.capacity t.data
  then (
    Chunk_array.add_chunk t.data;
    true)
  else false
;;

let create ~start_num_chunks ~max_num_chunks ~num_rows ~chunk_cols =
  if num_rows < 4 then raise_s [%message "num_rows must be >= 4" (num_rows : int)];
  if chunk_cols < 3 then raise_s [%message "chunk_cols must be >= 3" (chunk_cols : int)];
  let data =
    Chunk_array.make
      ~start_len:start_num_chunks
      ~rows:num_rows
      ~chunk_cols
      ~max_len:max_num_chunks
  in
  let t =
    { data
    ; player_chunk_ids = Hashtbl.create (module Client_id)
    ; row_filled_cells = Row_filled_cells_counter.make ~len:num_rows
    ; full_rows = Int_hash_set.create ()
    }
  in
  t
;;

let chunks_that_piece_is_inside t client_id =
  Hashtbl.find t.player_chunk_ids client_id |> Option.value ~default:[]
;;

let num_rows t = Chunk_array.get t.data 0 |> Chunk.num_rows
let num_cols t = (Chunk_array.get t.data 0 |> Chunk.num_cols) * Chunk_array.length t.data
let cols_per_chunk t = Chunk_array.get t.data 0 |> Chunk.num_cols

let in_bounds t coord =
  let max_col = num_cols t - 1 in
  let row, col = Coordinate.row_col coord in
  Int.between row ~low:0 ~high:(num_rows t - 1) && Int.between col ~low:0 ~high:max_col
;;

let get_chunk_index_and_coord t coord =
  let row, col = Coordinate.row_col coord in
  let chunk_i = col / cols_per_chunk t in
  chunk_i, Coordinate.make ~row ~col:(col - (chunk_i * cols_per_chunk t))
;;

let get t coord =
  let chunk_i, coord = get_chunk_index_and_coord t coord in
  let chunk = Chunk_array.get t.data chunk_i in
  Chunk.get chunk coord
;;

let is_filled_or_locked t coord =
  let tile = get t coord in
  Tile.is_filled tile || Tile.is_locked tile
;;

let set t coord tile =
  let chunk_i, chunk_coord = get_chunk_index_and_coord t coord in
  let chunk = Chunk_array.get t.data chunk_i in
  let row = Coordinate.row chunk_coord in
  if is_filled_or_locked t coord then Row_filled_cells_counter.decr t.row_filled_cells row;
  if Tile.is_filled tile || Tile.is_locked tile
  then Row_filled_cells_counter.incr t.row_filled_cells row;
  if Row_filled_cells_counter.get t.row_filled_cells row = num_cols t
  then Hash_set.add t.full_rows row;
  Chunk.set chunk chunk_coord tile
;;

let player_chunks t client_id =
  chunks_that_piece_is_inside t client_id |> List.map ~f:(Chunk_array.get t.data)
;;

let get_piece t client_id =
  match player_chunks t client_id with
  | [] -> None
  | chunk :: _ -> Chunk.get_piece chunk client_id |> Some
;;

let get_piece_exn t client_id = get_piece t client_id |> Option.value_exn

let remove_piece t client_id =
  player_chunks t client_id
  |> List.iter ~f:(fun chunk -> Chunk.remove_piece chunk client_id);
  Hashtbl.remove t.player_chunk_ids client_id
;;

let highest_non_empty_row t ~col =
  let chunk_i = col / cols_per_chunk t in
  let adj_col = col - (chunk_i * cols_per_chunk t) in
  let chunk = Chunk_array.get t.data chunk_i in
  Chunk.highest_non_empty_row chunk adj_col
;;

let set_piece t client_id piece =
  remove_piece t client_id;
  let chunk_ids =
    Piece.coordinates piece
    |> List.map ~f:(fun coord -> Coordinate.col coord / cols_per_chunk t)
    |> List.dedup_and_sort ~compare:Int.compare
  in
  Hashtbl.set t.player_chunk_ids ~key:client_id ~data:chunk_ids;
  List.map chunk_ids ~f:(Chunk_array.get t.data)
  |> List.iter ~f:(fun chunk -> Chunk.set_piece chunk client_id piece)
;;

let all_player_positions t =
  Hashtbl.keys t.player_chunk_ids
  |> List.map ~f:(fun client_id -> client_id, get_piece_exn t client_id)
;;

let piece_is_inside_filled_or_locked_tile t = Piece.exists ~f:(is_filled_or_locked t)

let update_row_filled_count_after_deleting_chunk t chunk_index =
  for
    col = chunk_index * cols_per_chunk t
    to (chunk_index * cols_per_chunk t) + cols_per_chunk t - 1
  do
    for row = 0 to num_rows t - 1 do
      set t (Coordinate.make ~row ~col) Tile.empty
    done
  done
;;

let delete_chunk t i =
  if Chunk_array.length t.data > 1 && i < Chunk_array.length t.data
  then (
    let players_to_move =
      Hashtbl.fold
        t.player_chunk_ids
        ~init:[]
        ~f:(fun ~key:client_id ~data:chunk_ids acc ->
          if List.exists chunk_ids ~f:(fun chunk_id -> chunk_id > i)
          then (client_id, get_piece_exn t client_id) :: acc
          else acc)
    in
    List.iter players_to_move ~f:(fun (client_id, _piece) -> remove_piece t client_id);
    update_row_filled_count_after_deleting_chunk t i;
    Chunk_array.delete t.data i;
    List.iter players_to_move ~f:(fun (client_id, piece) ->
      set_piece
        t
        client_id
        (Piece.move piece ~dir:(Coordinate.make ~row:0 ~col:(-cols_per_chunk t))));
    true)
  else false
;;

let fill_tile t coord =
  set t coord Tile.filled;
  true
;;

let valid_piece_position t piece =
  Piece.for_all piece ~f:(fun coord -> in_bounds t coord && get t coord |> Tile.is_empty)
;;

(* this is shit *)
let get_spawn_position t col piece_type =
  let desired_height_dif = 10 in
  let spawn_radius = 2 in
  let min_row, max_row, min_col, max_col =
    Piece.make ~piece_type ~pivot_position:Coordinate.origin ()
    |> Piece.fold ~init:(0, 0, 0, 0) ~f:(fun (min_row, max_row, min_col, max_col) coord ->
      let r, c = Coordinate.row_col coord in
      Int.min min_row r, Int.max max_row r, Int.min min_col c, Int.max max_col c)
  in
  let min_row, min_col = Int.abs min_row, Int.abs min_col in
  let max_row_spawn = num_rows t - 3 - max_row in
  let get_row_to_spawn_in col =
    if not (Int.between col ~low:0 ~high:(num_cols t - 1))
    then None
    else (
      let highest_non_empty =
        List.range
          (Int.max (col - min_col - spawn_radius) 0)
          (Int.min (col + max_col + spawn_radius + 1) (num_cols t))
        |> List.map ~f:(fun col -> highest_non_empty_row t ~col)
        |> List.map ~f:(Option.value ~default:0)
        |> List.max_elt ~compare:Int.compare
        |> Option.value_exn
      in
      let min_row_spawn = highest_non_empty + 1 + min_row in
      Option.some_if
        (min_row_spawn <= max_row_spawn)
        (Int.min (highest_non_empty + desired_height_dif + min_row) max_row_spawn))
  in
  let rec search i =
    let col = col + i in
    let next_i = if i = 0 then -1 else if i < 0 then -1 * i else -1 * (i + 1) in
    match get_row_to_spawn_in col with
    | None -> search next_i
    | Some row ->
      let pivot_position = Coordinate.make ~row ~col in
      let piece = Piece.make ~piece_type ~pivot_position () in
      if valid_piece_position t piece then pivot_position else search next_i
  in
  search 0
;;

let spawn_piece t client_id col piece_type =
  let pivot_position = get_spawn_position t col piece_type in
  let piece = Piece.make ~piece_type ~pivot_position () in
  set_piece t client_id piece;
  true
;;

let any_below_filled_or_locked_or_under_ground t piece =
  let new_piece = Piece.move piece ~dir:Coordinate.down in
  Piece.exists new_piece ~f:(fun coord -> Coordinate.row coord < 0)
  || piece_is_inside_filled_or_locked_tile t new_piece
;;

let lock_piece t client_id =
  let piece = get_piece_exn t client_id in
  Piece.iter piece ~f:(fun coord ->
    if get t coord |> Tile.is_empty
    then set t coord (Tile.locked (Piece.piece_type piece)));
  remove_piece t client_id
;;

let update_player_positions_after_row_delete t =
  let to_shift_down, to_lock =
    all_player_positions t
    |> List.fold
         ~init:([], [])
         ~f:(fun ((to_shift_down, to_lock) as acc) (client_id, piece) ->
           if Piece.exists piece ~f:(is_filled_or_locked t)
           then
             if any_below_filled_or_locked_or_under_ground t piece
             then to_shift_down, client_id :: to_lock
             else (client_id, piece) :: to_shift_down, to_lock
           else acc)
  in
  List.iter to_lock ~f:(lock_piece t);
  List.iter to_shift_down ~f:(fun (client_id, piece) ->
    set_piece t client_id (Piece.move piece ~dir:Coordinate.down))
;;

let delete_row t row =
  Chunk_array.iter t.data ~f:(fun chunk -> Chunk.delete_row chunk row);
  Row_filled_cells_counter.delete_and_add_row t.row_filled_cells row;
  Hash_set.remove t.full_rows row;
  (* why is there no [Hash_set.map_in_place] or [Hash_set.map]? :( *)
  Hash_set.filter t.full_rows ~f:(fun full_row -> full_row > row)
  |> Hash_set.iter ~f:(fun row ->
    Hash_set.remove t.full_rows row;
    Hash_set.add t.full_rows (row - 1));
  update_player_positions_after_row_delete t;
  true
;;

let get_full_rows t = Hash_set.to_list t.full_rows

(* OPTIMIZATION: check if the coordinate is higher than the highest filled row in the 
   column and skip the linear scan if it is. *)
let get_drop_count t coord =
  let row = Coordinate.row coord in
  let break = ref false in
  let row_dif = ref 0 in
  while not !break do
    if !row_dif = row
       || is_filled_or_locked
            t
            (Coordinate.add coord (Coordinate.make ~row:(-(!row_dif + 1)) ~col:0))
    then break := true
    else row_dif := !row_dif + 1
  done;
  !row_dif
;;

(* [piece] is the piece before [move] is applied. *)
let should_lock t piece move =
  if piece_is_inside_filled_or_locked_tile t piece
  then true
  else (
    match move with
    | Player_move.Drop -> true
    | Down -> any_below_filled_or_locked_or_under_ground t piece
    | Left | Right | Clockwise | Counter_clockwise -> false)
;;

let get_moved_piece t piece move =
  let new_piece =
    match move with
    | Player_move.Down ->
      if should_lock t piece Player_move.Down
      then piece
      else Piece.move piece ~dir:Coordinate.down
    | Left -> Piece.move piece ~dir:Coordinate.left
    | Right -> Piece.move piece ~dir:Coordinate.right
    | Clockwise -> Piece.rotate piece ~dir:`Clockwise
    | Counter_clockwise -> Piece.rotate piece ~dir:`Counter_clockwise
    | Drop ->
      let drop_count =
        Piece.coordinates piece
        |> List.sort_and_group ~compare:(fun c1 c2 ->
          let col1, col2 = Coordinate.(col c1, col c2) in
          Int.compare col1 col2)
        |> List.map ~f:(fun row_coords ->
          List.min_elt row_coords ~compare:(fun c1 c2 ->
            let row1, row2 = Coordinate.(row c1, row c2) in
            Int.compare row1 row2)
          |> Option.value_exn)
        |> List.map ~f:(get_drop_count t)
        |> List.min_elt ~compare:Int.compare
        |> Option.value_exn
      in
      Piece.move piece ~dir:(Coordinate.make ~row:(-drop_count) ~col:0)
  in
  if Piece.equal new_piece piece || valid_piece_position t new_piece
  then new_piece
  else piece
;;

let player_move t client_id move =
  match get_piece t client_id with
  | None -> false
  | Some old_piece ->
    if piece_is_inside_filled_or_locked_tile t old_piece
    then lock_piece t client_id
    else (
      let new_piece = get_moved_piece t old_piece move in
      set_piece t client_id new_piece;
      if should_lock t old_piece move then lock_piece t client_id);
    true
;;

let set_locked_piece t piece =
  Piece.coordinates piece
  |> List.filter ~f:(in_bounds t)
  |> List.iter ~f:(fun coord -> set t coord (Tile.locked (Piece.piece_type piece)));
  true
;;

let apply_action t = function
  | Action.Player_move (client_id, move) -> player_move t client_id move
  | Spawn_piece (client_id, coord, piece_type) -> spawn_piece t client_id coord piece_type
  | Set_player_piece (client_id, piece) ->
    set_piece t client_id piece;
    true
  | Set_locked_piece piece -> set_locked_piece t piece
  | Remove_piece client_id ->
    remove_piece t client_id;
    true
  | Delete_row row -> delete_row t row
  | Delete_chunk chunk_index -> delete_chunk t chunk_index
  | Add_chunk -> add_chunk t
  | Fill_tile coord -> fill_tile t coord
;;

let sorted_coordinate_list t =
  let filled_or_locked =
    List.cartesian_product (List.range 0 (num_rows t)) (List.range 0 (num_cols t))
    |> List.map ~f:(fun (row, col) -> Coordinate.make ~row ~col)
    |> List.map ~f:(fun coord -> coord, get t coord)
    |> List.filter ~f:(fun (_coord, tile) -> not (Tile.is_empty tile))
    |> List.map ~f:(fun (coord, tile) -> coord, Tile.to_char tile)
  in
  let falling =
    all_player_positions t
    |> List.map ~f:snd
    |> List.map ~f:(fun piece ->
      Piece.coordinates piece
      |> List.map ~f:(fun coord ->
        coord, Piece.piece_type piece |> Piece_type.to_char |> Char.lowercase))
    |> List.join
  in
  filled_or_locked @ falling
  |> List.sort ~compare:(fun (coord1, ch1) (coord2, ch2) ->
    if not (Coordinate.equal coord1 coord2)
    then Coordinate.compare coord1 coord2
    else Char.compare ch1 ch2)
;;

let to_string t =
  let tiles = sorted_coordinate_list t in
  let max_col, max_row =
    List.fold tiles ~init:(0, 0) ~f:(fun (max_col, max_row) (coord, _tile) ->
      let row, col = Coordinate.row_col coord in
      Int.max max_col col, Int.max row max_row)
  in
  let row_string row =
    List.range 0 (max_col + 1)
    |> List.map ~f:(fun col ->
      List.find tiles ~f:(fun (coord, _tile) ->
        Coordinate.equal coord (Coordinate.make ~row ~col))
      |> Option.map ~f:snd
      |> Option.value ~default:'.')
    |> String.of_list
  in
  List.range ~stride:(-1) max_row (-1)
  |> List.map ~f:row_string
  |> String.concat ~sep:"\n"
;;

let show t = to_string t |> print_endline

let%expect_test "applying actions" =
  let board = create ~start_num_chunks:1 ~max_num_chunks:1 ~chunk_cols:10 ~num_rows:6 in
  let _ = apply_action board (Action.Fill_tile Coordinate.origin) in
  let id = Client_id.create () in
  let _ = apply_action board (Action.Spawn_piece (id, 1, Piece_type.T)) in
  show board;
  [%expect {|
    ttt
    .t.
    ...
    #..
    |}];
  [ Action.Player_move (id, Player_move.Counter_clockwise)
  ; Player_move (id, Player_move.Left)
  ; Player_move (id, Player_move.Drop)
  ]
  |> List.iter ~f:(fun action ->
    let _ = apply_action board action in
    ());
  show board;
  [%expect {|
    T.
    TT
    T.
    #.
    |}];
  let _ = apply_action board (Action.Delete_row 1) in
  show board;
  [%expect {|
    T.
    TT
    #.
    |}]
;;

let%expect_test "adding chunk" =
  let board = create ~start_num_chunks:1 ~max_num_chunks:2 ~num_rows:4 ~chunk_cols:4 in
  let _ = apply_action board (Action.Fill_tile Coordinate.origin) in
  show board;
  [%expect {| # |}];
  let _ = apply_action board Action.Add_chunk in
  let _ = apply_action board (Action.Fill_tile (Coordinate.make ~row:3 ~col:7)) in
  show board;
  [%expect {|
    .......#
    ........
    ........
    #.......
    |}]
;;

let%expect_test "deleting first chunk" =
  let t = create ~start_num_chunks:2 ~max_num_chunks:2 ~num_rows:6 ~chunk_cols:4 in
  let _ = apply_action t (Action.Fill_tile Coordinate.origin) in
  let _ = apply_action t (Action.Fill_tile (Coordinate.make ~row:1 ~col:7)) in
  let client_id = Client_id.create () in
  let spawn_piece_action = Action.Spawn_piece (client_id, 4, Piece_type.O) in
  let _ = apply_action t spawn_piece_action in
  show t;
  print_s [%sexp (t.row_filled_cells |> Row_filled_cells_counter.to_array : int array)];
  [%expect
    {|
    ....oo..
    ....oo..
    .......#
    #.......
    (1 1 0 0 0 0)
    |}];
  let _ = apply_action t (Action.Delete_chunk 0) in
  show t;
  print_s [%sexp (t.row_filled_cells |> Row_filled_cells_counter.to_array : int array)];
  [%expect {|
    oo..
    oo..
    ...#
    ....
    (0 1 0 0 0 0)
    |}]
;;

let%expect_test "deleting second chunk" =
  let t = create ~start_num_chunks:2 ~max_num_chunks:2 ~num_rows:6 ~chunk_cols:4 in
  let _ = apply_action t (Action.Fill_tile Coordinate.origin) in
  let _ = apply_action t (Action.Fill_tile (Coordinate.make ~row:1 ~col:7)) in
  let client_id = Client_id.create () in
  let spawn_piece_action = Action.Spawn_piece (client_id, 1, Piece_type.O) in
  let _ = apply_action t spawn_piece_action in
  show t;
  print_s [%sexp (t.row_filled_cells |> Row_filled_cells_counter.to_array : int array)];
  [%expect
    {|
    .oo.....
    .oo.....
    .......#
    #.......
    (1 1 0 0 0 0)
    |}];
  let _ = apply_action t (Action.Delete_chunk 1) in
  show t;
  print_s [%sexp (t.row_filled_cells |> Row_filled_cells_counter.to_array : int array)];
  [%expect {|
    .oo
    .oo
    ...
    #..
    (1 0 0 0 0 0)
    |}]
;;

let%expect_test "spawning piece in full column" =
  let t = create ~start_num_chunks:1 ~max_num_chunks:1 ~num_rows:8 ~chunk_cols:15 in
  let client_id = Client_id.create () in
  let spawn_piece_type col piece_type =
    let _ = apply_action t (Action.Spawn_piece (client_id, col, piece_type)) in
    ()
  in
  let drop_piece () =
    let _ = apply_action t (Action.Player_move (client_id, Player_move.Drop)) in
    ()
  in
  Fn.apply_n_times
    ~n:3
    (fun () ->
      spawn_piece_type 4 Piece_type.O;
      drop_piece ())
    ();
  [%expect {| |}];
  Fn.apply_n_times
    ~n:3
    (fun () ->
      spawn_piece_type 4 Piece_type.O;
      drop_piece ())
    ();
  List.iter Piece_type.all ~f:(fun piece_type ->
    spawn_piece_type 4 piece_type;
    show t;
    print_endline "--------";
    remove_piece t client_id);
  [%expect
    {|
   OO..OO..iiii
   OO..OO......
   OO..OO......
   OO..OO......
   OO..OO......
   OO..OO......
   --------
   OO..OO..oo
   OO..OO..oo
   OO..OO....
   OO..OO....
   OO..OO....
   OO..OO....
   --------
   OO..OO..ttt
   OO..OO...t.
   OO..OO.....
   OO..OO.....
   OO..OO.....
   OO..OO.....
   --------
   OO..OO...ss
   OO..OO..ss.
   OO..OO.....
   OO..OO.....
   OO..OO.....
   OO..OO.....
   --------
   OO..OO..zz.
   OO..OO...zz
   OO..OO.....
   OO..OO.....
   OO..OO.....
   OO..OO.....
   --------
   OO..OO..jjj
   OO..OO....j
   OO..OO.....
   OO..OO.....
   OO..OO.....
   OO..OO.....
   --------
   OO..OO..lll
   OO..OO..l..
   OO..OO.....
   OO..OO.....
   OO..OO.....
   OO..OO.....
   --------
  |}]
;;

let%expect_test "getting full columns" =
  let t = create ~num_rows:10 ~start_num_chunks:1 ~max_num_chunks:1 ~chunk_cols:4 in
  List.range 0 4
  |> List.iter ~f:(fun col ->
    set t (Coordinate.make ~row:0 ~col) Tile.filled;
    set t (Coordinate.make ~row:3 ~col) Tile.filled);
  print_s [%sexp (t.full_rows : Int_hash_set.t)];
  [%expect {| (0 3) |}];
  let _ = delete_row t 0 in
  print_s [%sexp (t.full_rows : Int_hash_set.t)];
  [%expect {| (2) |}]
;;
