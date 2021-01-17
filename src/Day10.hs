module Day10 (solve1, solve2) where

import Data.Char (intToDigit)
import Data.String.Utils (strip)

{-|
  Solver for Day 10 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/10
-}

solve1 :: [Char] -> Int
solve1 = length . (!! 40) . iterate lookAndSay . strip

solve2 :: [Char] -> Int
solve2 = length . (!! 50) . iterate lookAndSay . strip

lookAndSay :: [Char] -> [Char]
lookAndSay [] = []
lookAndSay cs@(c:_) =
  let (same, rest) = span (== c) cs
  in intToDigit (length same) : c : lookAndSay rest
