open! Core
open Tetromino_civilization_common

type t =
  { predicted_board : Board.t
  ; confirmed_board : Board.t
  ; client_id : Client_id.t
  }
