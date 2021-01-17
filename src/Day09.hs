module Day09 (solve1, solve2) where

import Data.List (nub, permutations)
import Data.Map ((!))

import qualified Data.Map.Strict as M

{-|
  Solver for Day 9 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/9
-}

solve1 :: [Char] -> Int
solve1 input =
  let m = parseInput input
  in minimum $ tourLength m <$> permutations (getCities m)

solve2 :: [Char] -> Int
solve2 input =
  let m = parseInput input
  in maximum $ tourLength m <$> permutations (getCities m)

parseInput :: [Char] -> M.Map (String, String) Int
parseInput = M.fromList . map (parse . words) . lines
  where parse [c1, "to", c2, "=", d] = (orderedTuple c1 c2, read d)

orderedTuple :: Ord a => a -> a -> (a, a)
orderedTuple a b = if a < b then (a, b) else (b, a)

getCities :: M.Map (String, String) Int -> [String]
getCities = nub . concatMap (\(a, b) -> [a, b]) . M.keys

tourLength :: M.Map (String, String) Int -> [String] -> Int
tourLength m cs = sum $ zipWith (\a b -> m ! orderedTuple a b) cs (tail cs)
