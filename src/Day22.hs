{-# LANGUAGE DuplicateRecordFields #-}

module Day22 (solve1, solve2) where

import Data.Maybe (fromJust, mapMaybe)
import Data.List (foldl', sortOn, subsequences)
import Data.List.Split (splitOn)
import Data.Tuple (swap)

import Debug.Trace (traceShow, traceShowId)

import qualified Data.Heap as H

{-|
  Solver for Day 22 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/22
-}

data Turn = Player | Boss
  deriving (Eq, Show)

data State = State Turn Stats Stats [String]
  deriving Show

data Stats = Stats {
  hitPoints :: Int,
  mana      :: Int,
  damage    :: Int,
  armor     :: Int,
  effects   :: [(String, [(Stats, Stats) -> (Stats, Stats)])]
  }

data Spell = Spell {
  name   :: String,
  cost   :: Int,
  effect :: [(Stats, Stats) -> (Stats, Stats)]
  }

instance Show Stats where
  show Stats{hitPoints = hp, mana = m, damage = d, armor = a, effects = es} =
    "(Stats " <> show hp <> " " <> show m <> " " <> show d <> " " <> show a
    <> " " <> show (map fst es) <> ")"

baseStats :: Stats
baseStats = Stats {
  hitPoints    = 0,
  mana         = 0,
  damage       = 0,
  armor        = 0,
  effects = []
  }

spells :: [Spell]
spells = [
  Spell {
    name   = "Magic Missile",
    cost   = 53,
    effect = [\(s, t) -> (s, t{hitPoints = hitPoints t - 4})]
    },
  Spell {
    name   = "Drain",
    cost   = 73,
    effect = [\(s, t) -> (s{hitPoints = min 50 (hitPoints s + 2)},
                          t{hitPoints = hitPoints t - 2})]
    },
  Spell {
    name   = "Shield",
    cost   = 113,
    effect = [\(s, t) -> (s{armor = armor s + 7}, t), id, id, id, id,
              \(s, t) -> (s{armor = armor s - 7}, t)]
    },
  Spell {
    name   = "Poison",
    cost   = 173,
    effect = replicate 6 (\(s, t) -> (s, t{hitPoints = hitPoints t - 3}))
    },
  Spell {
    name   = "Recharge",
    cost   = 229,
    effect = replicate 5 (\(s, t) -> (s{mana = mana s + 101}, t))
    }
  ]

solve1 :: [Char] -> Int
solve1 = fromJust . uniformCostSearch expandFringe isPlayerWin
  . (\b -> State Player playerStats b []) . readStats
  where playerStats = baseStats { hitPoints = 50, mana = 500 }

solve2 :: [Char] -> Int
solve2 = fromJust . uniformCostSearch expandFringe isPlayerWin
  . (\b -> State Player playerStats b []) . readStats
  where
    slowDeath = cycle [\(s, t) -> (s{hitPoints = hitPoints s - 1}, t), id]
    playerStats = baseStats { hitPoints = 50, mana = 500,
      effects = [("Slow Death", slowDeath)] }

readStats :: String -> Stats
readStats input =
  let [[_, hp], [_, damage]] = splitOn ": " <$> lines input
  in baseStats { hitPoints = read hp, damage = read damage }

uniformCostSearch :: (State -> [(Int, State)]) -> (State -> Bool) -> State
                  -> Maybe Int
uniformCostSearch generateFn isGoal = search . H.singleton . H.Entry 0
  where
    search pq = case H.viewMin pq of
      Nothing -> Nothing
      Just (H.Entry cost curr, pq') ->
        if isGoal curr
        then Just cost
        else search $ H.union pq' $ H.fromList
          $ map (\(c, s) -> H.Entry (cost + c) s) $ generateFn curr

isPlayerWin :: State -> Bool
isPlayerWin (State _ player boss _) = isAlive player && not (isAlive boss)

isAlive :: Stats -> Bool
isAlive = (> 0) . hitPoints

expandFringe :: State -> [(Int, State)]
expandFringe (State turn player boss path) =
  case turn of
    Boss   -> [(0, State Player (boss' `attack` player') boss' path)]
    Player -> map (\(c, p, spell) -> (c, State Boss p boss' (spell:path)))
      $ mapMaybe (player' `cast`) spells
  where
    (player', boss') = uncurry applySpells $ swap $ applySpells boss player

    attack p1 p2 =
      if isAlive p1  -- Can't attack if already dead from spell damage
      then p2 { hitPoints = hitPoints p2 - max 1 (damage p1 - armor p2)}
      else p2

    cast p@Stats{mana = m, effects = es}
         Spell{name = spellName, cost = c, effect = e} =
      if isAlive p && c <= m && spellName `notElem` map fst es
      then Just (c, p { mana = m - c, effects = (spellName, e):es }, spellName)
      else Nothing

applySpells :: Stats -> Stats -> (Stats, Stats)
applySpells self target = (self' { effects = effs' }, target')
  where
    effs = effects self
    (self', target') = foldl' (\s (_, e:_) -> e s) (self, target) effs
    effs' = filter (not . null . snd) $ map (\(n, _:es) -> (n, es)) effs
