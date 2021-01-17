{-# LANGUAGE LambdaCase #-}

module Day08 (solve1, solve2) where

import Data.Char (isHexDigit)

{-|
  Solver for Day 8 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/8
-}

solve1 :: [Char] -> Int
solve1 input =
  let ls = lines input
  in sum (map length ls) - sum (map strLength ls)

solve2 :: [Char] -> Int
solve2 input =
  let ls = lines input
  in sum (map codeLength ls) - sum (map length ls)

strLength :: [Char] -> Int
strLength = subtract 2 . length'
  where
    hexPrefix = \case
      '\\':'x':d1:d2:t | isHexDigit d1 && isHexDigit d2 -> True
      _ -> False
    length' s = case s of
      []              -> 0
      '\\':'\\':t     -> 1 + length' t
      '\\':'"':t      -> 1 + length' t
      _ | hexPrefix s -> 1 + length' (drop 4 s)
      _:t             -> 1 + length' t

codeLength :: [Char] -> Int
codeLength s = 2 + length s + length (filter (`elem` "\"\\") s)
