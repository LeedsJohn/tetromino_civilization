open! Core
open! Async

let make_board rows cols =
  let ar = Array.make_matrix ~dimx:cols ~dimy:rows 0 in
  for x = 0 to cols - 1 do
    for y = 0 to rows - 1 do
      ar.(x).(y) <- Random.int 900 + 100
    done
  done;
  ar
;;

let time_fn f =
  let start_time = Time_ns.now () in
  let%bind.Deferred res = f () in
  print_s [%message "" (res : int)];
  let end_time = Time_ns.now () in
  let run_time =
    Time_ns.sub end_time (Time_ns.to_span_since_epoch start_time)
    |> Time_ns.to_span_since_epoch
    |> Time_ns.Span.to_string_hum
  in
  print_s [%message "" (run_time : string)];
  return ()
;;

let n_sum_row ar i = Array.sum (module Int) ar.(i) ~f:Fn.id
let n_sum ar = List.range 0 (Array.length ar) |> List.sum (module Int) ~f:(n_sum_row ar)

let a_sum_row ar i : int Deferred.t =
  Array.sum (module Int) ar.(i) ~f:Fn.id |> Deferred.return
;;

let a_sum ar =
  let%bind.Deferred nums =
    List.range 0 (Array.length ar)
    |> Deferred.List.map ~how:`Parallel ~f:(fun i -> a_sum_row ar i)
  in
  List.sum (module Int) nums ~f:Fn.id |> return
;;

let main () =
  let board = make_board 1000 100000 in
  let%bind.Deferred () =
    time_fn (fun () ->
      let res = n_sum board in
      return res)
  in
  time_fn (fun () -> a_sum board)
;;

let () =
  don't_wait_for (main ());
  never_returns (Scheduler.go ())
;;
