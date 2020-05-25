module Main where

import Prelude
import Control.Monad.State (evalStateT, gets, lift, modify_)
import Data.Map (Map, insert)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log, logShow)
import Storage (class Has, class PutStore, Entity(..), EntityCount, Global, System, cmap, exists, get, initStore, members, newEntity, set)
import Type.Prelude (Proxy(..))

data Position
  = Position Int

instance showPosition :: Show Position where
  show (Position num) = "(Position " <> show num <> ")"

data Velocity
  = Velocity Int

instance showVelocity :: Show Velocity where
  show (Velocity num) = "(Velocity " <> show num <> ")"

data World
  = World
    { entityCounter :: Global EntityCount
    , positions :: Map Entity Position
    , velocities :: Map Entity Velocity
    }

unWorld ::
  World ->
  { entityCounter :: Global EntityCount
  , positions :: Map Entity Position
  , velocities :: Map Entity Velocity
  }
unWorld (World world) = world

initWorld :: World
initWorld =
  World
    { entityCounter: initStore
    , positions: initStore # insert (Entity 5) (Position 10) # insert (Entity 6) (Position 6)
    , velocities: initStore # insert (Entity 5) (Velocity 5)
    }

initWorld2 :: World
initWorld2 =
  World
    { entityCounter: initStore
    , positions: initStore
    , velocities: initStore
    }

instance hasEntityCounter :: Has World EntityCount (Global EntityCount) where
  getStore _ = gets (unWorld >>> _.entityCounter)

instance putStoreEntityCounter :: PutStore World (Global EntityCount) where
  putStore entityCounter = modify_ (unWorld >>> _ { entityCounter = entityCounter } >>> World)

instance hasPosition :: Has World Position (Map Entity Position) where
  getStore _ = gets (unWorld >>> _.positions)

instance putStorePosition :: PutStore World (Map Entity Position) where
  putStore positions = modify_ (unWorld >>> _ { positions = positions } >>> World)

instance hasVelocity :: Has World Velocity (Map Entity Velocity) where
  getStore _ = gets (unWorld >>> _.velocities)

instance putStoreVelocity :: PutStore World (Map Entity Velocity) where
  putStore velocities = modify_ (unWorld >>> _ { velocities = velocities } >>> World)

runGame :: System World Int
runGame = do
  Position p1 /\ Velocity v1 <- get (Entity 5)
  set (Entity 5) $ (Position $ p1 + v1) /\ (Velocity $ v1 + 1)
  Position p2 /\ Velocity v2 <- get (Entity 5)
  lift $ logShow (p2 /\ v2)
  e <- exists (Entity 5) (Proxy :: Proxy Velocity)
  lift $ log $ "e: " <> show e
  e2 <- exists (Entity 6) (Proxy :: Proxy Position)
  lift $ log $ "e2: " <> show e2
  e3 <- exists (Entity 5) (Proxy :: Proxy (Tuple Position Velocity))
  lift $ log $ "e3: " <> show e3
  e4 <- exists (Entity 6) (Proxy :: Proxy (Tuple Position Velocity))
  lift $ log $ "e4: " <> show e4
  positionMembers <- members (Proxy :: Proxy Velocity)
  lift $ logShow positionMembers
  bothMembers <- members (Proxy :: Proxy (Tuple Position Velocity))
  lift $ log $ "bothMembers: " <> show bothMembers
  Position pBefore /\ Velocity vBefore <- get (Entity 5)
  lift $ log $ "position 5 before: " <> show pBefore
  lift $ log $ "velocity 5 before: " <> show vBefore
  cmap \(Position p) -> Position (p + 1)
  Position pAfter1 /\ Velocity vAfter1 <- get (Entity 5)
  lift $ log $ "position 5 after1: " <> show pAfter1
  lift $ log $ "velocity 5 after1: " <> show vAfter1
  cmap \(Position p /\ Velocity v) -> Position (p + v) /\ Velocity (v - 1)
  Position pAfter2 /\ Velocity vAfter2 <- get (Entity 5)
  lift $ log $ "position 5 after2: " <> show pAfter2
  lift $ log $ "velocity 5 after2: " <> show vAfter2
  ety1 <- newEntity (Position 3)
  lift $ log $ "ety1: " <> show ety1
  ety2 <- newEntity (Position 33)
  lift $ log $ "ety2: " <> show ety2
  Position ety2P <- get ety2
  lift $ log $ "ety2P: " <> show ety2P
  pure $ p2 + v2

runGame2 :: System World Int
runGame2 = do
  entity1 <- newEntity $ Position 5 /\ Velocity 2
  -- lift $ logShow entity1
  -- storage <- getStore (Proxy :: Proxy Position)
  -- lift $ logShow storage
  cmap \(Position p /\ Velocity v) -> Position (p + v) /\ Velocity (v - 1)
  Position p /\ Velocity v <- get entity1
  lift $ log $ "Position: " <> show p <> ", Velocity: " <> show v
  pure p

main :: Effect Unit
main = do
  p <- evalStateT runGame initWorld
  logShow p
