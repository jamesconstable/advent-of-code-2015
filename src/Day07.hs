module Day07 (solve1, solve2) where

import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR)
import Data.Char (isDigit)
import Data.List (foldl')
import Data.Map ((!))
import Debug.Trace (trace)

import qualified Data.Map.Strict as M

{-|
  Solver for Day 7 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/7
-}

data Input = Value Int | Wire String

data Component =
    Cached Int
  | Direct Input
  | AndGate Input Input
  | OrGate Input Input
  | NotGate Input
  | LShift Input Input
  | RShift Input Input

type Circuit = M.Map String Component

solve1 :: [Char] -> Int
solve1 = fst . calculate (Wire "a") . readCircuit

solve2 :: [Char] -> Int
solve2 input =
  let
    circuit = readCircuit input
    a = fst $ calculate (Wire "a") circuit
  in fst $ calculate (Wire "a") (M.insert "b" (Cached a) circuit)

calculate :: Input -> Circuit -> (Int, Circuit)
calculate (Value v) m = (v, m)
calculate (Wire w) m  = case m ! w of
  Cached v      -> (v, m)
  Direct i      -> let (v, m') = calculate i m
                   in (v, M.insert w (Cached v) m')
  AndGate i1 i2 -> let
                     (v1, m')  = calculate i1 m
                     (v2, m'') = calculate i2 m'
                     result    = v1 .&. v2
                   in (result, M.insert w (Cached result) m'')
  OrGate i1 i2  -> let
                     (v1, m')  = calculate i1 m
                     (v2, m'') = calculate i2 m'
                     result    = v1 .|. v2
                   in (result, M.insert w (Cached result) m'')
  NotGate i     -> let (v, m') = calculate i m
                   in (complement v, M.insert w (Cached (complement v)) m')
  LShift i1 i2  -> let
                     (v1, m')  = calculate i1 m
                     (v2, m'') = calculate i2 m'
                     result    = v1 `shiftL` v2
                   in (result, M.insert w (Cached result) m'')
  RShift i1 i2  -> let
                     (v1, m')  = calculate i1 m
                     (v2, m'') = calculate i2 m'
                     result    = v1 `shiftR` v2
                   in (result, M.insert w (Cached result) m'')

readCircuit :: [Char] -> M.Map String Component
readCircuit = M.fromList . map readComponent . lines
  where
    readInput raw | all isDigit raw = Value (read raw)
                  | otherwise       = Wire raw
    readComponent l = case words l of
      [i, "->", o]                -> (o, Direct (readInput i))
      [i1, "AND", i2, "->", o]    -> (o, AndGate (readInput i1) (readInput i2))
      [i1, "OR", i2, "->", o]     -> (o, OrGate (readInput i1) (readInput i2))
      ["NOT", i, "->", o]         -> (o, NotGate (readInput i))
      [i1, "LSHIFT", i2, "->", o] -> (o, LShift (readInput i1) (readInput i2))
      [i1, "RSHIFT", i2, "->", o] -> (o, RShift (readInput i1) (readInput i2))
