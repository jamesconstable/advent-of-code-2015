module Day11 (solve1, solve2) where

import Data.Char (chr, isLower, ord)
import Data.List (group)
import Data.List.HT (padLeft)
import Data.String.Utils (strip)
import Numeric (readInt, showIntAtBase)

{-|
  Solver for Day 11 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/11
-}

solve1 :: [Char] -> [Char]
solve1 = head . filter isValid . iterate increment . strip

solve2 :: [Char] -> [Char]
solve2 = (!! 1) . filter isValid . iterate increment . strip

passwordToInt :: [Char] -> Int
passwordToInt = fst . head . readInt 26 isLower (\c -> ord c - ord 'a')

intToPassword :: Int -> [Char]
intToPassword n = showIntAtBase 26 (chr . (+ ord 'a')) n ""

increment :: [Char] -> [Char]
increment p = padLeft 'a' (length p) $ intToPassword $ (+ 1) $ passwordToInt p

isValid :: [Char] -> Bool
isValid p
  | length p < 3 = False
  | otherwise    = condition1 && condition2 && condition3
  where
    condition1 = any (\(a, b, c) -> ord b - ord a == 1 && ord c - ord b == 1)
      $ zip3 p (tail p) (tail (tail p))
    condition2 = not $ any (`elem` "iol") p
    condition3 = (>= 2) $ length $ filter ((> 1) . length) $ group p
