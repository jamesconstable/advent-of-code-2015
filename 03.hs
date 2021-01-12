{-# LANGUAGE LambdaCase #-}

import Data.Map ((!))
import Data.Maybe (fromJust, maybe)

import qualified Data.Map as M
import qualified Data.Set as S

import Runner (runner)

{-|
  Solver for Day 3 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/3
-}

main :: IO ()
main = runner (solve 1) (solve 2)

solve :: Int -> [Char] -> Int
solve n = length . visitHouses n

visitHouses :: Int -> [Char] -> S.Set (Int, Int)
visitHouses n =
  visitHouses' 0 (M.fromList $ zip [0..n-1] $ repeat (0, 0)) S.empty
  where
    tupleAdd (a, b) (x, y) = (a+x, b+y)
    visitHouses' turn santas visited = \case
      []     -> visited'
      '<':ds -> continue (-1,  0) ds
      '>':ds -> continue ( 1,  0) ds
      '^':ds -> continue ( 0, -1) ds
      'v':ds -> continue ( 0,  1) ds
      where
        turn'          = (turn + 1) `mod` n
        visited'       = S.insert (santas ! turn) visited
        updateSantas d = M.adjust (tupleAdd d) turn santas
        continue d     = visitHouses' turn' (updateSantas d) visited'
