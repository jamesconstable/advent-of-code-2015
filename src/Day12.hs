{-# LANGUAGE LambdaCase #-}

module Day12 (solve1, solve2) where

import Data.Maybe (mapMaybe)
import Data.Ratio (denominator, numerator)

import qualified Text.JSON as J

{-|
  Solver for Day 12 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/12
-}

solve1 :: [Char] -> Integer
solve1 = sum . concatMap (getNumbers . fromOk . J.decode) . lines

solve2 :: [Char] -> Integer
solve2 = sum . concatMap (getNonRedNumbers . fromOk . J.decode) . lines

fromOk :: J.Result a -> a
fromOk (J.Ok r) = r

values :: J.JSObject J.JSValue -> [J.JSValue]
values = map snd . J.fromJSObject

getNumbers :: J.JSValue -> [Integer]
getNumbers = \case
  J.JSObject o     -> concatMap getNumbers (values o)
  J.JSArray es     -> concatMap getNumbers es
  J.JSRational _ r -> [numerator r `quot` denominator r]
  _                -> []

getNonRedNumbers :: J.JSValue -> [Integer]
getNonRedNumbers = \case
  J.JSObject o | isRed o -> []
  J.JSObject o           -> concatMap getNonRedNumbers (values o)
  J.JSArray es           -> concatMap getNonRedNumbers es
  J.JSRational _ r       -> [numerator r `quot` denominator r]
  _                      -> []
  where
    isRed = elem "red" . mapMaybe fromJSString . values
    fromJSString = \case
      J.JSString s -> Just $ J.fromJSString s
      _            -> Nothing
