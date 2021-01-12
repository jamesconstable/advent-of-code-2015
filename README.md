# Advent of Code 2015

Going back and completing the old
[Advent of Code](https://adventofcode.com) challenges. My goal for this one
is to work on quickly producing tidy Haskell solutions using only the libraries
included with GHC.

## Instructions

To compile a solver, run
```
ghc --make -no-keep-hi-files -no-keep-o-files <day_number>
```
replacing `<day_number>` with `01`, `02`, etc. This will produce an executable
with the same name in the current directory.

All solvers take a command line argument indicating which part to solve, and
read their input from stdin. For example, to run part 2 of the Day 1 solver with
the input in `input.txt`, type:
```
cat input.txt | ./01 2
```
