open! Core

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
  f ();
  let end_time = Time_ns.now () in
  let run_time =
    Time_ns.sub end_time (Time_ns.to_span_since_epoch start_time)
    |> Time_ns.to_span_since_epoch
    |> Time_ns.Span.to_string_hum
  in
  print_s [%sexp (run_time : string)]
;;

let shuffle_dumb ar shuffle_y =
  let rows = Array.length ar.(0) in
  for x = 0 to Array.length ar - 1 do
    for y = shuffle_y to rows - 2 do
      ar.(x).(y) <- ar.(x).(y + 1)
    done;
    ar.(x).(rows - 1) <- 0
  done
;;

let smarter_shuffle ar shuffle_y =
  let rows = Array.length ar.(0) in
  for x = 0 to Array.length ar - 1 do
    Array.blit
      ~src:ar.(x)
      ~src_pos:(shuffle_y + 1)
      ~dst:ar.(x)
      ~dst_pos:shuffle_y
      ~len:(rows - shuffle_y - 1);
    ar.(x).(rows - 1) <- 0
  done
;;

let bruh f =
  let ar = make_board 1000 10000 in
  let john () = f ar 0 in
  time_fn john
;;

let () =
  bruh shuffle_dumb;
  bruh smarter_shuffle
;;
