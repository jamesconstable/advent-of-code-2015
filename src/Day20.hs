{-# LANGUAGE TupleSections #-}

module Day20 (solve1, solve2) where

import Data.IntMap ((!))
import Data.List (find, foldl')
import Data.Maybe (fromJust)
import Data.String.Utils (strip)

import qualified Data.IntMap.Strict as IM

{-|
  Solver for Day 20 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/20
-}

solve1 :: [Char] -> Int
solve1 input = fromJust $ find ((>= n) . (street !)) [1..]
  where
    n = read $ strip input
    street = streetUpTo (n `quot` 10)

solve2 :: [Char] -> Int
solve2 input = fromJust $ find ((>= n) . (street !)) [1..]
  where
    n = read $ strip input
    street = streetUpTo' (n `quot` 11)

streetUpTo :: Int -> IM.IntMap Int
streetUpTo n = foldl' elfVisit (IM.fromList $ map (, 0) [1..n]) [1..n]
  where elfVisit s i = foldl' (flip (IM.adjust (+ i*10))) s [i,i+i..n]

streetUpTo' :: Int -> IM.IntMap Int
streetUpTo' n = foldl' elfVisit (IM.fromList $ map (, 0) [1..n]) [1..n]
  where elfVisit s i = foldl' (flip (IM.adjust (+ i*11))) s (take 50 [i,i+i..n])
