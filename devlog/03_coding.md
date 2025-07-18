# Coding for real

7/17/2025

I am kind of overwhelmed right now by the amount of things to keep track of, but I've
finally gotten some stuff down. I wrote the first draft of a `Tile_grid` module. The
number of instructions to delete an entry in each column roughly scales with
`min(row, heighest row - row)`, which I am happy with because I am anticipating most row
deletions coming at the top or bottom of the stack depending on decisions I make later.
Obviously, there's still room to make stuff faster.

One of the next things I need to think about is how I'm going to remove chunks when the
number of players online is small. I want to remove chunks that have no players around
them. I don't mind this operation scaling with the number of columns, but it would be sad
for this to be a `rows x columns` operation (which I'm trying to avoid in general).

Maybe a good next step would be to expand the `Tile_grid` to create either a `Chunk` or
`Board` module depending on how I go about things.

Creating a `Chunk` should be easy enough, but I'd need to think about how that's actually
going to be useful.

Another problem I'm thinking about is how I want to represent moves. The client and server
should be sending patches to each other. I could either make this look like "make this
coordinate equal to this tile," but I think that it would probably be better to have a set
of possible instructions like "spawn tile at coordinate," "move falling tile down," 
"rotate falling tile," etc.
