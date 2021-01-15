module Main where

import System.Environment (getArgs)

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04

solvers = [
  [Day01.solve1, Day01.solve2],
  [Day02.solve1, Day02.solve2],
  [Day03.solve1, Day03.solve2],
  [Day04.solve1, Day04.solve2]]

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
  run $ solvers !! (read day - 1) !! (read part - 1)