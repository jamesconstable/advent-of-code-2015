{-# LANGUAGE DuplicateRecordFields #-}

module Day22 (solve1, solve2) where

import Data.Bifunctor (second)
import Data.Maybe (fromJust, mapMaybe)
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Tuple (swap)

import qualified Data.Heap as H

{-|
  Solver for Day 22 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/22
-}

data Stats = Stats {
  hitPoints :: Int,
  mana      :: Int,
  damage    :: Int,
  armor     :: Int,
  actions   :: [Action],
  effects   :: [Effect]
  }

data Turn = Player | Boss
  deriving Eq

data State = State Turn Stats Stats

-- Effects have a name and a list of functions that are applied one-per-turn
-- until the list is exhausted (after which the Effect ends). 'Instant' spells
-- and actions are represented as single-turn Effects. Function tuples are of
-- the form (self, target).
data Effect = Effect String [(Stats, Stats) -> (Stats, Stats)]

instance Eq Effect where
  Effect n1 _ == Effect n2 _ = n1 == n2

data Action = Action {
  cost   :: Int,
  effect :: Effect
  }

solve1 :: [Char] -> Int
solve1 = minimumWinCost basePlayer . readBossStats

solve2 :: [Char] -> Int
solve2 = minimumWinCost basePlayer { effects = [hardMode] } . readBossStats
  where
    hardMode = Effect "Hard Mode" $
      cycle [\(s, t) -> (s { hitPoints = hitPoints s - 1 }, t), id]

baseStats :: Stats
baseStats = Stats {
  hitPoints = 0,
  mana      = 0,
  damage    = 0,
  armor     = 0,
  actions   = [],
  effects   = []
  }

basePlayer :: Stats
basePlayer = baseStats {
  hitPoints = 50,
  mana = 500,
  actions = [
    Action {
      cost   = 53,
      effect = Effect "Magic Missile"
        [\(s, t) -> (s, t { hitPoints = hitPoints t - 4 })]
      },
    Action {
      cost   = 73,
      effect = Effect "Drain"
        [\(s, t) -> (s { hitPoints = hitPoints s + 2 },
                     t { hitPoints = hitPoints t - 2 })]
      },
    Action {
      cost   = 113,
      effect = Effect "Shield"
        [\(s, t) -> (s { armor = armor s + 7 }, t), id, id, id, id,
         \(s, t) -> (s { armor = armor s - 7 }, t)]
      },
    Action {
      cost   = 173,
      effect = Effect "Poison"
        (replicate 6 (\(s, t) -> (s, t { hitPoints = hitPoints t - 3 })))
      },
    Action {
      cost   = 229,
      effect = Effect "Recharge"
        (replicate 5 (\(s, t) -> (s { mana = mana s + 101 }, t)))
      }
    ]
  }

readBossStats :: String -> Stats
readBossStats input =
  let [[_, hp], [_, d]] = splitOn ": " <$> lines input
  in baseStats {
    hitPoints = read hp,
    damage = read d,
    actions = [
      Action {
        cost   = 0,
        effect = Effect "Attack" [\(s, t) ->
          (s, t { hitPoints = hitPoints t - max 1 (damage s - armor t) })]
        }
      ]
    }

minimumWinCost :: Stats -> Stats -> Int
minimumWinCost player boss = fromJust
  $ uniformCostSearch expandFringe isPlayerWin $ State Player player boss

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
isPlayerWin (State _ player boss) = isAlive player && isDead boss

isAlive :: Stats -> Bool
isAlive = (> 0) . hitPoints

isDead :: Stats -> Bool
isDead = not . isAlive

expandFringe :: State -> [(Int, State)]
expandFringe (State turn player boss)
  -- If the player is already dead once effects have been applied, prune the
  -- game tree at this point.
  | isDead player' = []

  -- If the boss dies from effects, don't take any actions, but still add the
  -- outcome to the game tree so the win condition will be triggered.
  | isDead boss' = [(0, State turn' player' boss')]

  | otherwise = case turn of
      Boss   -> second (\b -> State turn' player' b) <$> tryAllActions boss'
      Player -> second (\p -> State turn' p boss') <$> tryAllActions player'

  where
    (player', boss') = uncurry applyActions $ swap $ applyActions boss player
    turn' = if turn == Player then Boss else Player
    tryAllActions p = mapMaybe (p `tryAction`) (actions p)

tryAction :: Stats -> Action -> Maybe (Int, Stats)
tryAction p Action{cost = c, effect = e} =
  -- To be successful, the player must (a) have enough mana, and (b) not have a
  -- prior invocation of that action still in effect.
  if c <= mana p && e `notElem` effects p
  then Just (c, p { mana = mana p - c, effects = e : effects p })
  else Nothing

applyActions :: Stats -> Stats -> (Stats, Stats)
applyActions self target = (self' { effects = effs' }, target')
  where
    effs = effects self
    (self', target') = foldl' (\s (Effect _ (e:_)) -> e s) (self, target) effs
    effs' = filter notEnded $ map (\(Effect n (_:es)) -> Effect n es) effs

notEnded :: Effect -> Bool
notEnded (Effect _ es) = not (null es)
