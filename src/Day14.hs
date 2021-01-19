module Day14 (solve1, solve2) where

import Data.List (foldl')
import Data.Map ((!))

import qualified Data.Map.Strict as M

{-|
  Solver for Day 14 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/14
-}

data Reindeer = Reindeer {
  getName     :: String,
  getSpeed    :: Int,
  getRaceTime :: Int,
  getRestTime :: Int
  }

solve1 :: [Char] -> Int
solve1 = maximum . map (distanceAfter 2503) . parseInput

solve2 :: [Char] -> Int
solve2 d = maximum $ M.elems $ tallyScores $ take 2503 $ map snapshotAt [1..]
  where
    reindeer = parseInput d
    snapshotAt t = map (\r -> (getName r, distanceAfter t r)) reindeer

parseInput :: [Char] -> [Reindeer]
parseInput = map (parseLine . words) . lines
  where
    parseLine [n,_,_,s,_,_,t,_,_,_,_,_,_,r,_] =
      Reindeer n (read s) (read t) (read r)

distanceAfter :: Int -> Reindeer -> Int
distanceAfter t (Reindeer _ speed raceTime restTime) =
  let
    cycleTime = raceTime + restTime
    cycleDistance = raceTime * speed
  in (t `quot` cycleTime) * cycleDistance
    + speed * min raceTime (t `mod` cycleTime)

tallyScores :: [[(String, Int)]] -> M.Map String Int
tallyScores = foldl' (M.unionWith (+)) M.empty . map score

score :: [(String, Int)] -> M.Map String Int
score snapshot =
  let dMax = maximum $ map snd snapshot
  in M.fromList $ map (\(r, d) -> (r, fromEnum (d == dMax))) snapshot
