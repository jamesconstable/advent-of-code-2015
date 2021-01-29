module Main where

import System.Environment (getArgs)

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19

{-|
   Provides a common interface for running solvers.
   Expects two command line arguments indicating which day (1 to 25) and
   which part (1 or 2) to run, reads input from stdin, and writes result to
   stdout.
-}
main :: IO ()
main = do
  let run fn = getContents >>= print . fn
  [day, part] <- getArgs
  case (read day, read part) of
    ( 1, 1) -> run Day01.solve1;  ( 1, 2) -> run Day01.solve2
    ( 2, 1) -> run Day02.solve1;  ( 2, 2) -> run Day02.solve2
    ( 3, 1) -> run Day03.solve1;  ( 3, 2) -> run Day03.solve2
    ( 4, 1) -> run Day04.solve1;  ( 4, 2) -> run Day04.solve2
    ( 5, 1) -> run Day05.solve1;  ( 5, 2) -> run Day05.solve2
    ( 6, 1) -> run Day06.solve1;  ( 6, 2) -> run Day06.solve2
    ( 7, 1) -> run Day07.solve1;  ( 7, 2) -> run Day07.solve2
    ( 8, 1) -> run Day08.solve1;  ( 8, 2) -> run Day08.solve2
    ( 9, 1) -> run Day09.solve1;  ( 9, 2) -> run Day09.solve2
    (10, 1) -> run Day10.solve1;  (10, 2) -> run Day10.solve2
    (11, 1) -> run Day11.solve1;  (11, 2) -> run Day11.solve2
    (12, 1) -> run Day12.solve1;  (12, 2) -> run Day12.solve2
    (13, 1) -> run Day13.solve1;  (13, 2) -> run Day13.solve2
    (14, 1) -> run Day14.solve1;  (14, 2) -> run Day14.solve2
    (15, 1) -> run Day15.solve1;  (15, 2) -> run Day15.solve2
    (16, 1) -> run Day16.solve1;  (16, 2) -> run Day16.solve2
    (17, 1) -> run Day17.solve1;  (17, 2) -> run Day17.solve2
    (18, 1) -> run Day18.solve1;  (18, 2) -> run Day18.solve2
    (19, 1) -> run Day19.solve1;  (19, 2) -> run Day19.solve2
    _ -> error "Unrecognised day/part number"
