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
  name     :: String,
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
  Item Weapon "Dagger"       8 4 0,
  Item Weapon "Shortsword"  10 5 0,
  Item Weapon "Warhammer"   25 6 0,
  Item Weapon "Longsword"   40 7 0,
  Item Weapon "Greataxe"    74 8 0,
  Item Armor  "Leather"     13 0 1,
  Item Armor  "Chainmail"   31 0 2,
  Item Armor  "Splintmail"  53 0 3,
  Item Armor  "Bandedmail"  75 0 4,
  Item Armor  "Platemail"  102 0 5,
  Item Ring   "Damage +1"   25 1 0,
  Item Ring   "Damage +2"   50 2 0,
  Item Ring   "Damage +3"  100 3 0,
  Item Ring   "Defense +1"  20 0 1,
  Item Ring   "Defense +2"  40 0 2,
  Item Ring   "Defense +3"  80 0 3
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
    $ sortOn (negate . comboCost) validCombos

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
