module Day25 (solve1, solve2) where

import Data.List (find)
import Data.Maybe (fromJust)

{-|
  Solver for Day 25 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/25
-}

solve1 :: String -> Int
solve1 input =
  let answerCoord = readInput input
  in snd $ fromJust $ find ((== answerCoord) . fst) codes

solve2 :: String -> String
solve2 = const "The End, no Part 2 :)"

readInput :: String -> (Int, Int)
readInput input =
  let ws = words input
  in (read $ init $ ws !! 15, read $ init $ ws !! 17)

codes :: [((Int, Int), Int)]
codes = ((1, 1), 20151125) : map generateNext codes
  where
    generateNext ((row, col), n) = ((row', col'), n')
      where
        row' = if row == 1 then col + 1 else row - 1
        col' = if row == 1 then 1 else col + 1
        n'   = n * 252533 `mod` 33554393
