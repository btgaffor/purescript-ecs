module Main where

import Prelude

import Control.Monad.State (evalStateT, gets, modify_)
import Data.Map (Map)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Storage (class Has, class PutStore, Entity, EntityCount, Global, SystemT, cmap, get, initStore, newEntity)

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

runGame :: SystemT World Effect Int
runGame = do
  entity1 <- newEntity $ Position 5 /\ Velocity 2

  cmap \(Position p /\ Velocity v) -> Position (p + v) /\ Velocity (v - 1)
  Position p /\ Velocity v <- get entity1

  pure p

main :: Effect Unit
main = do
  let
    (wut :: Effect Int) = evalStateT runGame initWorld
  p <- evalStateT runGame initWorld
  log $ "Expected 7, Got: " <> show p
