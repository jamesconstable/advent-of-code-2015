module Day16 (solve1, solve2) where

import Data.List.Split (splitOn)
import Data.Map ((!))

import qualified Data.Map.Strict as M

{-|
  Solver for Day 16 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/16
-}

solve1 :: [Char] -> Int
solve1 = fst . head . filter (matches sample . snd) . parseInput

solve2 :: [Char] -> Int
solve2 = fst . head . filter (matches2 sample . snd) . parseInput

sample :: M.Map String Int
sample = M.fromList [
  ("children", 3),
  ("cats", 7),
  ("samoyeds", 2),
  ("pomeranians", 3),
  ("akitas", 0),
  ("vizslas", 0),
  ("goldfish", 5),
  ("trees", 3),
  ("cars", 2),
  ("perfumes", 1)
  ]

parseInput :: [Char] -> [(Int, M.Map String Int)]
parseInput = map parseLine . lines
  where
    parseLine l =
      let (_:_:_:_:sue, _:_:items) = break (== ':') l
      in (read sue, parseItems items)
    parseItems =
      M.fromList . map ((\[i, n] -> (i, read n)) . splitOn ": ") . splitOn ", "

matches :: M.Map String Int -> M.Map String Int -> Bool
matches full partial = M.intersection full partial == partial

matches2 :: M.Map String Int -> M.Map String Int -> Bool
matches2 full partial = lowerBounds && upperBounds && exactMatches
  where
    checkWith op =
      all (\f -> f `M.notMember` partial || (full ! f) `op` (partial ! f))
    lowerBounds = checkWith (<) ["cats", "trees"]
    upperBounds = checkWith (>) ["pomeranians", "goldfish"]
    exactMatches = checkWith (==)
      ["children", "samoyeds", "akitas", "vizslas", "cars", "perfumes"]
