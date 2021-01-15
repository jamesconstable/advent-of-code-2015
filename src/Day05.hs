module Day05 (solve1, solve2) where

import Data.List (isInfixOf, tails)

{-|
  Solver for Day 5 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/5
-}

solve1 :: [Char] -> Int
solve1 = length . filter isNice . lines
  where
    isNice cs =
      let
        condition1 = 3 <= length (filter (`elem` "aeiou") cs)
        condition2 = any (uncurry (==)) $ zip cs (tail cs)
        condition3 = not $ any (`isInfixOf` cs) ["ab", "cd", "pq", "xy"]
      in condition1 && condition2 && condition3

solve2 :: [Char] -> Int
solve2 = length . filter isNice . lines
  where
    isNice cs =
      let
        windowTails = map (splitAt 2) $ filter ((>= 4) . length) (tails cs)
        condition1 = any (uncurry isInfixOf) windowTails
        condition2 = any (uncurry (==)) $ zip cs (tail (tail cs))
      in condition1 && condition2
