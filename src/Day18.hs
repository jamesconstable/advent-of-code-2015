module Day18 (solve1, solve2) where

import Control.Monad (guard)
import Data.List (foldl')
import Data.Map ((!), (!?))
import Data.Maybe (mapMaybe)
import Relude.Extra.Tuple (toSnd)

import qualified Data.Map.Strict as M

{-|
  Solver for Day 18 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/18
-}

data Light = On | Off deriving Eq

type LightGrid = M.Map (Int, Int) Light

solve1 :: [Char] -> Int
solve1 = countOn . M.elems . (!! 100) . iterate update . parseInput

solve2 :: [Char] -> Int
solve2 = countOn . M.elems . (!! 100) . iterate (setCornersOn . update)
  . setCornersOn . parseInput

parseInput :: String -> LightGrid
parseInput = M.fromList
  . concat
  . zipWith (\y l -> map (\(x, v) -> ((y, x), v)) l) [0..]
  . map (zipWith (\x v -> (x, if v == '#' then On else Off)) [0..])
  . lines

neighbours :: (Int, Int) -> LightGrid -> [Light]
neighbours p m = mapMaybe ((m !?) . tupleAdd p) $ do
  y <- [-1, 0, 1]
  x <- [-1, 0, 1]
  guard (not (y == 0 && x == 0))
  return (y, x)

tupleAdd :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
tupleAdd (a, b) (x, y) = (a + x, b + y)

update :: LightGrid -> LightGrid
update m = M.fromList $ map (toSnd (conwayCellStep m)) $ M.keys m

conwayCellStep :: LightGrid -> (Int, Int) -> Light
conwayCellStep m p = case (m ! p, countOn (neighbours p m)) of
  (On,  2) -> On
  (On,  3) -> On
  (Off, 3) -> On
  _        -> Off

setCornersOn :: LightGrid -> LightGrid
setCornersOn m = foldl' (\m p -> M.insert p On m) m
  [(0, 0), (0, 99), (99, 0), (99, 99)]

countOn :: [Light] -> Int
countOn = length . filter (== On)
