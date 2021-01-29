module Day19 (solve1, solve2) where

import Data.List (nub)
import Data.List.HT (takeWhileJust)
import Data.List.Split (splitOn)
import Data.List.Utils (join)

{-|
  Solver for Day 19 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/19
-}

solve1 :: [Char] -> Int
solve1 = length . uncurry generate . parseInput

solve2 :: [Char] -> Int
solve2 = const 0

parseInput :: String -> ([(String, String)], String)
parseInput input = (replacements, molecule)
  where
    [replacementLines, [molecule]] = splitOn [""] $ lines input
    replacements = map ((\[a, b] -> (a, b)) . splitOn " => ") replacementLines

splitsOn :: Eq a => [a] -> [a] -> [([a], [a])]
splitsOn delim s = map (\(xs, bs) -> (join delim (reverse bs), join delim xs))
  $ init $ tail $ takeWhileJust $ iterate (>>= zipperNext)
  $ Just (splitOn delim s, [])

zipperNext :: ([a], [a]) -> Maybe ([a], [a])
zipperNext (t, bs) = case t of
  []   -> Nothing
  x:xs -> Just (xs, x:bs)

generate :: [(String, String)] -> String -> [String]
generate subs s =
  nub $ concatMap (\(p, r) -> map (\(h, t) -> h <> r <> t) (splitsOn p s)) subs
