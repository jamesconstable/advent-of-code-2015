{-# LANGUAGE TemplateHaskell #-}

module Day22 (solve1, solve2) where

import Control.Lens ((^.), (%~), (+~), (-~), _1, _2, makeLenses)
import Data.Function ((&))
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, mapMaybe)
import Data.Tuple (swap)

import qualified Data.Heap as H

{-|
  Solver for Day 22 of the Advent of Code 2015
  Problem description: https://adventofcode.com/2015/day/22
-}

data Turn = Player | Boss
  deriving Eq

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

data Stats = Stats {
  _hitPoints :: Int,
  _mana      :: Int,
  _damage    :: Int,
  _armor     :: Int,
  _actions   :: [Action],
  _effects   :: [Effect]
  }

makeLenses ''Stats

data State = State Turn Stats Stats

solve1 :: [Char] -> Int
solve1 = minimumWinCost basePlayer . readBossStats

solve2 :: [Char] -> Int
solve2 = minimumWinCost basePlayer { _effects = [hardMode] } . readBossStats
  where hardMode = Effect "Hard Mode" $ cycle [_1.hitPoints -~ 1, id]

baseStats :: Stats
baseStats = Stats {
  _hitPoints = 0,
  _mana      = 0,
  _damage    = 0,
  _armor     = 0,
  _actions   = [],
  _effects   = []
  }

basePlayer :: Stats
basePlayer = baseStats {
  _hitPoints = 50,
  _mana      = 500,
  _actions   = [
    Action {
      cost   = 53,
      effect = Effect "Magic Missile" [_2.hitPoints -~ 4]
      },
    Action {
      cost   = 73,
      effect = Effect "Drain" [(_1.hitPoints +~ 2) . (_2.hitPoints -~ 2)]
      },
    Action {
      cost   = 113,
      effect = Effect "Shield" [_1.armor +~ 7, id, id, id, id, _1.armor -~ 7]
      },
    Action {
      cost   = 173,
      effect = Effect "Poison" $ replicate 6 (_2.hitPoints -~ 3)
      },
    Action {
      cost   = 229,
      effect = Effect "Recharge" $ replicate 5 (_1.mana +~ 101)
      }
    ]
  }

readBossStats :: String -> Stats
readBossStats input =
  let [[_, hp], [_, d]] = splitOn ": " <$> lines input
  in baseStats {
    _hitPoints = read hp,
    _damage    = read d,
    _actions   = [
      Action {
        cost   = 0,
        effect = Effect "Attack"
          [\(s, t) -> (s, t & hitPoints -~ max 1 (s^.damage - t^.armor))]
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
isAlive = (> 0) . _hitPoints

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
      Boss   -> (_2 %~ \b -> State turn' player' b) <$> tryAllActions boss'
      Player -> (_2 %~ \p -> State turn' p boss') <$> tryAllActions player'

  where
    (player', boss') = uncurry applyActions $ swap $ applyActions boss player
    turn' = if turn == Player then Boss else Player
    tryAllActions p = mapMaybe (p `tryAction`) (p^.actions)

tryAction :: Stats -> Action -> Maybe (Int, Stats)
tryAction p Action{cost = c, effect = e} =
  -- To be successful, the player must (a) have enough mana, and (b) not have a
  -- prior invocation of that action still in effect.
  if c <= p^.mana && e `notElem` p^.effects
  then Just (c, p & mana -~ c & effects %~ (e:))
  else Nothing

applyActions :: Stats -> Stats -> (Stats, Stats)
applyActions self target = (self' { _effects = effs' }, target')
  where
    effs = self^.effects
    (self', target') = foldl' (\s (Effect _ (e:_)) -> e s) (self, target) effs
    effs' = filter notEnded $ map (\(Effect n (_:es)) -> Effect n es) effs

notEnded :: Effect -> Bool
notEnded (Effect _ es) = not (null es)
