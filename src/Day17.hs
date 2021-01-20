module Day17 (solve1, solve2) where

import Data.IntMap ((!))
import Relude.Extra.Tuple (toSnd)

import qualified Data.IntMap.Lazy as IM
import qualified Data.Set as S

{-|
  Solver for Day 17 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/17
-}

data Container = Container {
  getId     :: Int,
  getVolume :: Int
  } deriving (Eq, Ord)

solve1 :: [Char] -> Int
solve1 = length . (! 150) . combinationsByVolume . parseInput

solve2 :: [Char] -> Int
solve2 input =
  let
    combinations = S.toList $ (! 150) $ combinationsByVolume $ parseInput input
    minContainers = minimum $ map length combinations
  in length $ filter ((== minContainers) . length) combinations

parseInput :: String -> [Container]
parseInput = zipWith (\i n -> Container i (read n)) [0..] . lines

combinationsByVolume :: [Container] -> IM.IntMap (S.Set (S.Set Container))
combinationsByVolume cs = cached
  where
    maxSum = sum $ map getVolume cs
    cached = IM.fromList
      $ (0, S.singleton S.empty) : map (toSnd combinationsFor) [1..maxSum]
    combinationsFor n =
      S.unions $ map (\c -> updateWith c $ getCached (n - getVolume c)) cs
    getCached n = IM.findWithDefault S.empty n cached
    updateWith c = S.map (S.insert c) . S.filter (S.notMember c)
