module Day24 (solve1, solve2) where

import Data.Bifunctor (bimap)
import Data.List.Extra (minimumOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)

{-|
  Solver for Day 24 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/24
-}

solve1 :: [Char] -> Int
solve1 = product . head . bestConfiguration 3 . map read . lines

solve2 :: [Char] -> Int
solve2 = product . head . bestConfiguration 4 . map read . lines

{-
  Returns the lowest-scoring partitioning of the given items into n groups.
  This function returns a full configuration, although it turns out that just
  generating the first partition would have been sufficient for the actual
  puzzle input. In general, however, it's possible to construct examples where
  the items remaining after a valid first partition do not partition evenly.
  E.g. given n = 3 and items = [1,2,3,5,6,7], a valid first partition is
  [1,2,5], but there's no way to partition the remaining items into two groups
  that each sum to 8.
-}
bestConfiguration :: Int -> [Int] -> [[Int]]
bestConfiguration n items =
  minimumOn score
  $ mapMaybe (\(p, r) -> (p :) <$> listToMaybe (equalPartitions (n-1) r))
  $ partitionsOfTotal (sum items `quot` n) items

{-|
  Returns all sublists with the specified total and their complements.
  E.g. partitionsOfTotal 5 [2..5] = [([2,3],[4,5]),([5],[2,3,4])]
-}
partitionsOfTotal :: Int -> [Int] -> [([Int], [Int])]
partitionsOfTotal n l = case compare n 0 of
  LT -> []
  EQ -> [([], l)]
  GT -> case compare (sum l) n of
    LT -> []
    EQ -> [(l, [])]
    GT -> concatMap
      (\(bs, e:es) -> map (bimap (e:) (bs <>)) (partitionsOfTotal (n-e) es))
      (init $ splits l)

{-|
  Returns all n-way partitionings of a list where each partition has the same
  sum. If the sum of the input list is not divisible by n or the items cannot be
  partitioned evenly, an empty list is returned.
-}
equalPartitions :: Int -> [Int] -> [[[Int]]]
equalPartitions n es
  | n == 1              = [[es]]
  | sum es `mod` n /= 0 = []
  | otherwise           = concatMap
      (\(p, leftovers) -> (p:) <$> equalPartitions (n-1) leftovers)
      $ partitionsOfTotal (sum es `quot` n) es

splits :: [a] -> [([a], [a])]
splits es = map (`splitAt` es) [0..length es]

score :: [[Int]] -> (Int, Int)
score (front:_) = (length front, product front)
