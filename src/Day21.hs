{-# LANGUAGE DuplicateRecordFields #-}

module Day21 (solve1, solve2) where

import Data.List (sortOn, subsequences)
import Data.List.Split (splitOn)

{-|
  Solver for Day 21 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/21
-}

data Outcome = PlayerWin | BossWin
  deriving Eq

data ItemType = Weapon | Armor | Ring
  deriving Eq

data Item = Item {
  itemType :: ItemType,
  cost     :: Int,
  damage   :: Int,
  armor    :: Int
  }

data Stats = Stats {
  hitPoints :: Int,
  damage    :: Int,
  armor     :: Int
  }

itemAllowance :: [(ItemType, (Int, Int))]
itemAllowance = [
  (Weapon, (1, 1)),
  (Armor,  (0, 1)),
  (Ring,   (0, 2))
  ]

shop :: [Item]
shop = [
  Item Weapon 8   4 0,
  Item Weapon 10  5 0,
  Item Weapon 25  6 0,
  Item Weapon 40  7 0,
  Item Weapon 74  8 0,
  Item Armor  13  0 1,
  Item Armor  31  0 2,
  Item Armor  53  0 3,
  Item Armor  75  0 4,
  Item Armor  102 0 5,
  Item Ring   25  1 0,
  Item Ring   50  2 0,
  Item Ring   100 3 0,
  Item Ring   20  0 1,
  Item Ring   40  0 2,
  Item Ring   80  0 3
  ]

solve1 :: [Char] -> Int
solve1 input =
  let bossStats = readStats input
  in comboCost $ head
    $ filter ((== PlayerWin) . fightOutcome bossStats . comboStats)
    $ sortOn comboCost validCombos

solve2 :: [Char] -> Int
solve2 input =
  let bossStats = readStats input
  in comboCost $ head
    $ filter ((== BossWin) . fightOutcome bossStats . comboStats)
    $ sortOn ((0 -) . comboCost) validCombos

readStats :: String -> Stats
readStats input =
  let [[_, hp], [_, damage], [_, armor]] = splitOn ": " <$> lines input
  in Stats (read hp) (read damage) (read armor)

validCombos :: [[Item]]
validCombos = filter isValidCombo $ subsequences shop

isValidCombo :: [Item] -> Bool
isValidCombo c = all satisfies itemAllowance
  where
    satisfies (t, r) = inRange r $ length $ filter ((== t) . itemType) c
    inRange (lower, upper) x = lower <= x && x <= upper

comboCost :: [Item] -> Int
comboCost = sum . map cost

comboDamage :: [Item] -> Int
comboDamage = sum . map (damage :: Item -> Int)

comboArmor :: [Item] -> Int
comboArmor = sum . map (armor :: Item -> Int)

comboStats :: [Item] -> Stats
comboStats c = Stats 100 (comboDamage c) (comboArmor c)

fightOutcome :: Stats -> Stats -> Outcome
fightOutcome = fightOutcome' True
  where
    fightOutcome' playerFirst boss player
      | hitPoints boss   <= 0 = PlayerWin
      | hitPoints player <= 0 = BossWin
      | otherwise = fightOutcome' (not playerFirst) boss' player'
      where
        boss'   = if playerFirst then player `attack` boss else boss
        player' = if playerFirst then player else boss `attack` player

    attack Stats{damage = d1} p2@Stats{hitPoints = hp2, armor = a2} =
      p2 { hitPoints = hp2 - max 1 (d1 - a2) }
