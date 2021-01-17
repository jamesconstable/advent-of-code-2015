{-# LANGUAGE LambdaCase #-}

module Day07 (solve1, solve2) where

import Data.Bits ((.&.), (.|.), complement, shiftL, shiftR)
import Data.Char (isDigit)
import Data.Map ((!))

import qualified Data.Map.Lazy as M

{-|
  Solver for Day 7 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/7
-}

data Input = Value Int | Wire String

data Component =
    Direct Input
  | AndGate Input Input
  | OrGate Input Input
  | NotGate Input
  | LShift Input Input
  | RShift Input Input

solve1 :: [Char] -> Int
solve1 = calculate (Wire "a") . readCircuit

solve2 :: [Char] -> Int
solve2 input =
  let
    circuit = readCircuit input
    a = calculate (Wire "a") circuit
  in calculate (Wire "a") $ M.insert "b" (Direct (Value a)) circuit

calculate :: Input -> M.Map String Component -> Int
calculate i m = calculate' i
  where
    cached = M.fromList $ map (\k -> (k, calculate' (Wire k))) (M.keys m)
    fromInput = \case
      Value v -> v
      Wire w  -> cached ! w
    calculate' = \case
      Value v -> v
      Wire w  -> case m ! w of
        Direct i      -> fromInput i
        AndGate i1 i2 -> fromInput i1 .&. fromInput i2
        OrGate i1 i2  -> fromInput i1 .|. fromInput i2
        NotGate i     -> complement (fromInput i)
        LShift i1 i2  -> fromInput i1 `shiftL` fromInput i2
        RShift i1 i2  -> fromInput i1 `shiftR` fromInput i2

readCircuit :: [Char] -> M.Map String Component
readCircuit = M.fromList . map readComponent . lines
  where
    readInput raw | all isDigit raw = Value (read raw)
                  | otherwise       = Wire raw
    readComponent l = case words l of
      [i, "->", o]                -> (o, Direct  (readInput i))
      [i1, "AND", i2, "->", o]    -> (o, AndGate (readInput i1) (readInput i2))
      [i1, "OR", i2, "->", o]     -> (o, OrGate  (readInput i1) (readInput i2))
      ["NOT", i, "->", o]         -> (o, NotGate (readInput i))
      [i1, "LSHIFT", i2, "->", o] -> (o, LShift  (readInput i1) (readInput i2))
      [i1, "RSHIFT", i2, "->", o] -> (o, RShift  (readInput i1) (readInput i2))
