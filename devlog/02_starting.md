# Starting

7/14/2025

I have a decent idea of what I want to make, so I am going to write some code today. I
think that I'll start by actually figuring out the API that I want to have and then maybe
I'll start with the server.

Here's how I imagine client interface looking:

```ocaml
(* client.mli *)

type t

val spawn : unit -> t Deferred.Or_error.t

val move : t -> Move.t -> unit Deferred.Or_error.t

val board_state : t -> Board.t

val rows_fill_percentage : t -> float array
```

And the server:

```ocaml
(* server.mli *)

type t

val create : unit -> t

val get_spawn_point : t -> Coordinate.t

val apply_move : t -> Client_id.t -> Move.t -> unit
```

There's gonna be a lot more going on behind the scenes and I have a lot of unanswered
questions. But I think the only way to figure out what those questions are is to get
started.
