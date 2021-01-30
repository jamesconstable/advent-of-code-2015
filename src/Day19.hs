module Day19 (solve1, solve2) where

import Data.List (nub, sortOn)
import Data.List.HT (takeWhileJust)
import Data.List.Split (splitOn)
import Data.List.Utils (join)
import Data.Maybe (fromJust, listToMaybe, mapMaybe)
import Data.Tuple (swap)

{-|
  Solver for Day 19 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/19
-}

solve1 :: [Char] -> Int
solve1 = length . uncurry generate . parseInput

solve2 :: [Char] -> Int
solve2 input = fromJust $ findByGreedyReplace subs' "e" molecule
  where
    (subs, molecule) = parseInput input
    -- Reverse the substitution rules to generate backwards from the goal (the
    -- search space is smaller in this direction), and reorder so the ones that
    -- result in the greatest length reduction appear first.
    subs' = sortOn (\(f, s) -> length s - length f) $ map swap subs

parseInput :: String -> ([(String, String)], String)
parseInput input = (subs, molecule)
  where
    [subLines, [molecule]] = splitOn [""] $ lines input
    subs = map ((\[a, b] -> (a, b)) . splitOn " => ") subLines

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

findByGreedyReplace :: [(String, String)] -> String -> String -> Maybe Int
findByGreedyReplace subs goal = findByGreedyReplace' 0
  where
    findByGreedyReplace' d curr = case generate subs curr of
      [] -> Nothing
      gs -> if goal `elem` gs
            then Just (d+1)
            else listToMaybe $ mapMaybe (findByGreedyReplace' (d+1)) gs

