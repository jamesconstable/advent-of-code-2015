{-# LANGUAGE LambdaCase #-}

module Day15 (solve1, solve2) where

import SimpleCmd (removeSuffix)

{-|
  Solver for Day 15 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/15
-}

data CookieSpec = CookieSpec {
  capacity   :: Int,
  durability :: Int,
  flavor     :: Int,
  texture    :: Int,
  calories   :: Int
  }

solve1 :: [Char] -> Int
solve1 = maximum . map cookieScore . generateRecipes 100 . parseInput

solve2 :: [Char] -> Int
solve2 = maximum . map cookieScore . filter ((== 500) . calories)
  . generateRecipes 100 . parseInput

parseInput :: [Char] -> [CookieSpec]
parseInput = map (parseLine . words) . lines
  where
    parseLine [_,_,c1,_,d,_,f,_,t,_,c2] =
      CookieSpec (parse c1) (parse d) (parse f) (parse t) (parse c2)
    parse = read . removeSuffix ","

generateRecipes :: Int -> [CookieSpec] -> [CookieSpec]
generateRecipes n = \case
  []   -> [CookieSpec 0 0 0 0 0]
  s:ss -> concatMap (\i -> add (mult i s) <$> generateRecipes (n-i) ss) [0..n]
  where
    add (CookieSpec cap1 d1 f1 t1 cal1) (CookieSpec cap2 d2 f2 t2 cal2) =
      CookieSpec (cap1+cap2) (d1+d2) (f1+f2) (t1+t2) (cal1+cal2)
    mult n (CookieSpec cap d f t cal) =
      CookieSpec (n*cap) (n*d) (n*f) (n*t) (n*cal)

cookieScore :: CookieSpec -> Int
cookieScore (CookieSpec c d f t _) = max 0 c * max 0 d * max 0 f * max 0 t
