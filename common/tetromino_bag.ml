open! Core

type t =
  { bag : Piece_type.t array
  ; mutable capacity : int
  }
[@@deriving bin_io, sexp_of]

let create () = { bag = Piece_type.all |> List.to_array; capacity = 7 }

let index_of { bag; capacity = _ } piece_type =
  Array.findi_exn bag ~f:(fun _i pt -> Piece_type.equal piece_type pt) |> fst
;;

let mem t piece_type = index_of t piece_type < t.capacity

let remove t piece_type =
  if t.capacity = 1
  then t.capacity <- 7
  else (
    let i = index_of t piece_type in
    t.bag.(i) <- t.bag.(t.capacity - 1);
    t.bag.(t.capacity - 1) <- piece_type;
    t.capacity <- t.capacity - 1)
;;

let get t =
  let i = Random.int 7 in
  let res = t.bag.(i) in
  remove t res;
  res
;;

let%expect_test "removing piece" =
  let t = create () in
  List.iter Piece_type.all ~f:(fun piece_type ->
    let before = mem t piece_type in
    remove t piece_type;
    let after = mem t piece_type in
    print_s [%message "" (before : bool) (after : bool)]);
  (* the last "after" is true because the bag is refilled after the last piece is
       removed. *)
  [%expect
    {|
    ((before true) (after false))
    ((before true) (after false))
    ((before true) (after false))
    ((before true) (after false))
    ((before true) (after false))
    ((before true) (after false))
    ((before true) (after true))
    |}]
;;
