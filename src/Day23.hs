{-# LANGUAGE LambdaCase #-}

module Day23 (solve1, solve2) where

import Data.IntMap ((!?))

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

{-|
  Solver for Day 23 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/23
-}

data Instruction =
    Hlf Char
  | Tpl Char
  | Inc Char
  | Jmp Int
  | Jie Char Int
  | Jio Char Int
  deriving Eq

type Program = IM.IntMap Instruction

solve1 :: [Char] -> Int
solve1 = getRegister 'b' . run 0 M.empty . readProgram

solve2 :: [Char] -> Int
solve2 = getRegister 'b' . run 0 (M.singleton 'a' 1) . readProgram

readProgram :: String -> Program
readProgram = IM.fromList . zip [0..] . map readInstruction . lines
  where
    readInstruction = \case
      'h':'l':'f':_:[r]     -> Hlf r
      't':'p':'l':_:[r]     -> Tpl r
      'i':'n':'c':_:[r]     -> Inc r
      'j':'m':'p':_:n       -> Jmp (parseOffset n)
      'j':'i':'e':_:r:_:_:n -> Jie r (parseOffset n)
      'j':'i':'o':_:r:_:_:n -> Jio r (parseOffset n)
    parseOffset = \case
      '+':n -> read n
      '-':n -> negate (read n)

run :: Int -> M.Map Char Int -> Program -> M.Map Char Int
run i rs p = case p !? i of
  Just (Hlf r)   -> run (i+1) (M.insertWith (\_ v -> v `quot` 2) r 0 rs) p
  Just (Tpl r)   -> run (i+1) (M.insertWith (\_ v -> v * 3) r 0 rs) p
  Just (Inc r)   -> run (i+1) (M.insertWith (\_ v -> v + 1) r 1 rs) p
  Just (Jmp o)   -> run (i+o) rs p
  Just (Jie r o) -> run (if even (getRegister r rs) then i+o else i+1) rs p
  Just (Jio r o) -> run (if getRegister r rs == 1 then i+o else i+1) rs p
  Nothing        -> rs

getRegister :: Char -> M.Map Char Int -> Int
getRegister = M.findWithDefault 0
