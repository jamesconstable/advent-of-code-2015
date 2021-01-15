{-# LANGUAGE LambdaCase #-}

module Day06 (solve1, solve2) where

import Data.Ix (range)
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Map ((!))

import qualified Data.Map.Strict as M

{-|
  Solver for Day 6 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/6
-}

type Point = (Int, Int)

data Instruction = On Point Point | Off Point Point | Toggle Point Point

solve1 :: [Char] -> Int
solve1 =
  length . filter id . M.elems . foldl' update M.empty . parseInstructions
  where
    update lights = \case
      On s e     -> foldl' turnOn lights $ range (s, e)
      Off s e    -> foldl' turnOff lights $ range (s, e)
      Toggle s e -> foldl' toggle lights $ range (s, e)
    turnOn m k = M.insert k True m
    turnOff m k = M.insert k False m
    toggle m k = M.insertWith (\_ o -> not o) k True m

solve2 :: [Char] -> Int
solve2 =
  sum . M.elems . foldl' update M.empty . parseInstructions
  where
    update lights = \case
      On s e     -> foldl' turnOn lights $ range (s, e)
      Off s e    -> foldl' turnOff lights $ range (s, e)
      Toggle s e -> foldl' toggle lights $ range (s, e)
    turnOn m k = M.insertWith (\_ o -> o + 1) k 1 m
    turnOff m k = M.insertWith (\_ o -> min 0 (o - 1)) k 0 m
    toggle m k = M.insertWith (\_ o -> o + 2) k 2 m

parseInstructions :: [Char] -> [Instruction]
parseInstructions = map (parse . words) . lines
  where
    parse = \case
      "turn":"on":t  -> uncurry On (parseRange t)
      "turn":"off":t -> uncurry Off (parseRange t)
      "toggle":t     -> uncurry Toggle (parseRange t)
    parseRange [start, "through", end] =
      let
        [sx, sy] = splitOn "," start
        [ex, ey] = splitOn "," end
      in ((read sx, read sy), (read ex, read ey))
