# Advent of Code 2023

I'm using Haskell again for this year's challenge! I've also decided to comment my solutions so that I can still
understand my code ten minutes after writing it, but I'm also trying to add things that might be useful for learners
(like me!). As the days go on, I'll probably end up only commenting on the more advanced concepts, so there'll be a kind
of progression. I'm no expert myself though, so I can't promise that everything will be optimal or clean!

🎄🎁 :)

## Background

This is only my second year doing Advent of Code with Haskell, but I've always liked programming puzzles. I'm also a fan
of both golfing and writing clean, maintanable code, which sort of puts me at odds with myself. 

This year, I've decided to mostly try and get the line count down, but I'll take a few extra bytes or lines over
significantly less readable code. This entails things like (ab)using built-in typeclass instances for built-in types
(horrible but much shorter) instead of defining my own types and instances (nice but much longer), but not making
something pointfree and unreadable to save a few characters.

## Running the code

I'm not sure how things work on Windows, but there's a simple Bash script in this directory that uses
[Stack](https://github.com/commercialhaskell/stack/) to run a single solution. Run `DayNN.hs` with `./run NN` (e.g.
`./run 08` will run `Day08.hs`).