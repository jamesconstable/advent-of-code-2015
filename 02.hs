import Data.List (isPrefixOf, sort)
import Data.Maybe (fromJust)

import Runner (runner)

{-|
  Solver for Day 2 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/2
-}

main :: IO ()
main = runner solve1 solve2

solve1 :: [Char] -> Int
solve1 = sum . map squareFootage . readDimensions

solve2 :: [Char] -> Int
solve2 = sum . map ribbonLength . readDimensions

readDimensions :: [Char] -> [[Int]]
readDimensions = map (sort . map read . splitOn "x") . lines

squareFootage :: [Int] -> Int
squareFootage [a, b, c] = 3*a*b + 2*a*c + 2*b*c

ribbonLength :: [Int] -> Int
ribbonLength [a, b, c] = 2*a + 2*b + a*b*c

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn delim s = splitOn' (s, [])
  where
    splitOn' (t@(~(x:xs)), bs)
      | null t               = [reverse bs]
      | delim `isPrefixOf` t = reverse bs : splitOn' (drop (length delim) t, [])
      | otherwise            = splitOn' (xs, x:bs)

