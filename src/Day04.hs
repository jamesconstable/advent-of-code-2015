module Day04 (solve1, solve2) where

import Data.Hash.MD5 (Str(..), md5s)
import Data.String.Utils (startswith, strip)

{-|
  Solver for Day 4 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/4
-}

solve1 :: [Char] -> Int
solve1 = findForHashPrefix "00000"

solve2 :: [Char] -> Int
solve2 = findForHashPrefix "000000"

findForHashPrefix :: [Char] -> [Char] -> Int
findForHashPrefix prefix base =
  let md5Input = Str . (strip base <>) . show
  in head $ dropWhile (not . startswith prefix . md5s . md5Input) [1..]
