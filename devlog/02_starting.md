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

(* In practice, I expect this to be batched *)
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

## Random mechanics and algorithms I've been thinking about

### New player joins

* Players should be spawned in a region where there are other active players
* Add new chunks if necessary

### Player leaving

* Drop their currently falling piece and don't spawn a new one
* I don't want to immediately remove unneeded chunks. Instead, I'll check every X minutes
  and then rebalance.

### Deleting a row

I think a naive "shuffle every row down" will be too slow for the server. I ran a test and
it took 32 ms on a 10000 x 1000 board which is too slow. I think that a shuffle down
algorithm will probably be fine for the client because they've gotta update the view
anyway and I assume that's more expensive...?

For the server, I could track how many rows have been deleted at or below each y level.
So, when the client references the coordinate (x, y), the server will translate this
coordinate to (x, y + deleted_rows[y])
