# Advent of Code 2015

Going back and completing the old
[Advent of Code](https://adventofcode.com) challenges. My goal for this one
is to work on quickly producing tidy Haskell solutions using any of the publicly
available libraries.

## Instructions

To compile, run
```
stack build
```

The resulting executable takes two command line argument indicating which day
and part to solve, and reads its input from stdin. For example, to run part 2
of day 1 with the input in `input.txt`, type:
```
stack run 1 2 < input.txt
```
