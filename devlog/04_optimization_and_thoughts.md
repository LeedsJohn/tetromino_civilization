# Optimization and thoughts

7/18/2025


## "Optimization"

I made a small optimization for the `delete` operation for `Column.t`s. Now, I only
shuffle values up until the lowest filled in value rather than all the way down to the
0th row. This probably won't do anything in practice, but it felt like I should do this
to maintain symmetry because that's what I do at the top.

## Thoughts

When I look at the big picture, this feels like a very simple project. But when I think
about it more, stuff gets complicated.

Here's a series of descriptions that get increasingly more detailed:

### One sentence overview:

The clients play moves and the server keeps track of the board.

### Slightly more detail:

Upon joining, the client will make a request a spawn location from the server. Then, the
client will send its moves to the server. Every x milliseconds, the server will send back
the moves in the chunks that are close to the client to ensure that they agree.

The client will be responsible for choosing which tile and where to place it, but the
spawn location must be the same as the server expects and must conform to bag spawning
(create a bag of 2 (i think?) of every piece, use that bag up, and then create a new bag).

The server maintains a queue of operations to apply - these operations can be moves from
clients, deletion of rows, addition / deletion of columns, spawning tiles to fill in
holes, etc.

**Other Concerns:**

* Rate limiting - i'd be excited if someone wrote a bot but i want to cap piece placement
  at a reasonable limit.
* Ummm

### Where to go next?

I feel like a lot of this logic can be shared. Maybe the next thing would be to use the
`Tile_grid` module to create a `Board` that can apply Tetris moves (movement / rotation).
After that, it would be simple to make something someone could play (singleplayer).

I really just need to get something down, but that is feeling a little bit intimidating
at the moment. I have 90 more minutes on a plane, but I'm tired so I might wait until
tomorrow.
