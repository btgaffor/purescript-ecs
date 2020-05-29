module Main2 where

import Prelude

import Control.Monad.Cont (ContT(..), runContT)
import Control.Monad.State (evalStateT, gets, lift, modify_)
import Data.Map (Map)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log, logShow)
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

runSubGame :: SystemT World (ContT Unit Effect) Unit
runSubGame = do
  entity <- newEntity $ Position 5
  cmap $ \(Position p) -> Position (p + 1)
  Position p <- get entity
  lift <<< lift $ logShow p
  pure unit

runGame :: SystemT World (ContT Unit Effect) Unit
runGame = do
  runSubGame
  entity1 <- newEntity $ Position 5 /\ Velocity 2

  cmap \(Position p /\ Velocity v) -> Position (p + v) /\ Velocity (v - 1)
  Position p /\ Velocity v <- get entity1

  value <- lift getAction
  lift <<< lift $ logShow value

main2 :: Effect Unit
main2 = do
  runContT (evalStateT runGame initWorld) pure

-- main :: Effect Unit
-- main =
--   runContT func pure
--   where
--     func :: ContT Unit Effect Unit
--     func = do
--       action <- getAction
--       lift $ logShow action

getAction :: ContT Unit Effect Int
getAction = ContT $ \dispatch -> dispatch 3
