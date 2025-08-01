open! Core
open Tetromino_civilization_common

module Action_list_array = Circular_array.Make (struct
    type t = int * Action.t [@@deriving bin_io, sexp_of]

    let copy = Fn.id
  end)

type t = Action_list_array.t

let make len = Action_list_array.make ~max_len:len ~start_len:len

let add t move_count action =
  Action_list_array.delete t 0;
  Action_list_array.append_some t (move_count, action)
;;

let get_actions_after t n =
  Action_list_array.to_some_list t
  |> List.filter ~f:(fun (move_count, _action) -> move_count > n)
;;
