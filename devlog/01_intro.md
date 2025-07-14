# Intro

7/13/2025

# Tetromino Civilization

After listening to the episode of [Corecursive](https://corecursive.com/one-million-checkboxes-with-nolen-royalty/)
with [Nolen Royalty](https://eieio.games/) and reading his new post about [a million chessboards](https://eieio.games/blog/a-million-realtime-chess-boards-in-a-single-process/),
I was inspired to make something similar. I love these projects because there is no designated
goal, and each player can decide their own goal. I would love to create a similar platform,
and this is my attempt at that.

My vision is a game of falling [tetrominos](https://en.wikipedia.org/wiki/Tetromino). Players
will share a board that is a happy medium size where you can carve out your own space or
interact with others. Once a row is completely full, it disappears.

## Goals

Here's some goals that I have:

* Support 100 concurrent players
* Have 10 players on the website at the same time
* Learn some stuff
* Document the development process
    * I'm anticipating writing a little bit most days and there will be no proof reading

# Technical outline

I'm going to be using [OxCaml](https://oxcaml.org/), Jane Street's extensions to OCaml.
OCaml is the language that I know the best and I love writing it. OxCaml is newly released
so I'm anticipating running into some problems with that, but I'll cross that bridge when
I get to it.

I'll use [Bonsai](https://github.com/janestreet/bonsai) for the front end for similar
reasons. I am not very good at Bonsai but I'd like to practice.

### Initial Architectural Vision

Due to how stuff works out, probably none of this will come true. But here's how I see
this playing out:

### Web Client

The user will have a view of the board around them. They'll see the pieces that currently
exist on the board, their currently falling piece, and other players' falling pieces.

There will be 6 possible moves: move left/right/down, rotate left/right, and drop piece.
With this limited move set, hopefully I'll be able to make something that mobile users
can play too.

Players will immediately see their moves on the board, but the server will track the
actual board state. I anticipate keeping the client and server in sync being a challenge.

### Server

As previously mentioned, the server will track the state of the board.

Some other responsibilities will be:

* Choosing where a new player should join
* Sending clients the initial state of the board
* Growing and shrinking the board based on the number of active players
* Other stuff that I haven't thought of.

My initial thoughts are to divide the board into `NxN` chunks (I feel like `N = 16` is a good
number). Then, the client will send their moves to the server and the server will send back
a patch saying "this is the state of the board." This patch can be sent to everybody who
is currently viewing the chunk (or neighboring chunks). Once the client receives the patch,
they may have to rectify their view of the game.

This is not a full explanation, but it's pretty much the extent of my current
idea.

### Misc

Players will never be able to cooperate enough to fill in complete rows. To compensate
for this, I am going to fill in random squares that are inaccessible (prioritizing squares
towards the bottom). I'll need to have some scheme for accelerating and decelerating the rate
at which I fill in holes.

### Board Size

I'll have to think about this more, but I think it makes sense to start with roughly 20
columns for the first few players and then backing off from there. For example, if there
are 100 active players, I don't think it makes sense to have 20,000 columns because I want
to group people kind of close together. Maybe like 8,000 would be a good number so that
way people could find their own space but there would definitely be some people sharing
space.

Growing should be easy enough - I can just add to the left and / or right boundary.
Shrinking might be slightly more complicated. I think that it makes sense to try and get
rid of chunks that don't currently have any active players.
