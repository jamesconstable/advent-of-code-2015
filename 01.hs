{-# LANGUAGE LambdaCase #-}

import Data.Maybe (fromJust)

import Runner (runner)

{-|
  Solver for Day 1 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/1
-}

main :: IO ()
main = runner solve1 solve2

solve1 :: [Char] -> Int
solve1 input = length (filter (== '(') input) - length (filter (== ')') input)

solve2 :: [Char] -> Int
solve2 = fromJust . navigate 0 1
  where
    navigate floor i = \case
      []     -> Nothing
      (d:ds) ->
        let floor' = floor + if d == '(' then 1 else -1
        in if floor' == -1 then Just i else navigate floor' (i+1) ds
