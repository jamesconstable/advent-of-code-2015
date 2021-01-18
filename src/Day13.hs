{-# LANGUAGE LambdaCase #-}

module Day13 (solve1, solve2) where

import Data.List (foldl', nub, permutations)
import Data.List.HT (rotate)
import Data.Map ((!))

import qualified Data.Map.Strict as M

{-|
  Solver for Day 13 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/13
-}

solve1 :: [Char] -> Int
solve1 = fst . optimalArrangement . parseInput

solve2 :: [Char] -> Int
solve2 = fst . optimalArrangement . addMe . parseInput

parseInput :: [Char] -> M.Map (String, String) Int
parseInput = M.fromList . map (parseLine . words) . lines
  where
    parseLine = \case
      [p1,_,"gain",n,_,_,_,_,_,_,p2] -> ((p1, init p2), read n)
      [p1,_,"lose",n,_,_,_,_,_,_,p2] -> ((p1, init p2), -read n)

optimalArrangement :: M.Map (String, String) Int -> (Int, [String])
optimalArrangement m = maximum $ map (\a -> (score a, a)) arrangements
  where
    arrangements = permutations $ nub $ map fst $ M.keys m
    score a = sum $ zipWith (\a b -> m!(a, b) + m!(b, a)) a (rotate 1 a)

addMe :: M.Map (String, String) Int -> M.Map (String, String) Int
addMe m = M.union m myPairings
  where
    people = nub $ map fst $ M.keys m
    myPairings =
      M.fromList $ concatMap (\p -> [(("Me", p), 0), ((p, "Me"), 0)]) people
