module Example where

import Prelude

import Control.Monad.Cont (ContT(..), runContT)
import Control.Monad.State (evalStateT, gets, lift, modify_)
import Data.Map (Map)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (logShow)
import Ecs (class GetStore, class SaveStore, Entity, EntityCount, Global, SystemT, cmap, get, initStore, newEntity)

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

instance hasEntityCounter :: GetStore World EntityCount (Global EntityCount) where
  getStore _ = gets (unWorld >>> _.entityCounter)

instance saveStoreEntityCounter :: SaveStore World (Global EntityCount) where
  saveStore entityCounter = modify_ (unWorld >>> _ { entityCounter = entityCounter } >>> World)

instance hasPosition :: GetStore World Position (Map Entity Position) where
  getStore _ = gets (unWorld >>> _.positions)

instance saveStorePosition :: SaveStore World (Map Entity Position) where
  saveStore positions = modify_ (unWorld >>> _ { positions = positions } >>> World)

instance hasVelocity :: GetStore World Velocity (Map Entity Velocity) where
  getStore _ = gets (unWorld >>> _.velocities)

instance saveStoreVelocity :: SaveStore World (Map Entity Velocity) where
  saveStore velocities = modify_ (unWorld >>> _ { velocities = velocities } >>> World)

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

main :: Effect Unit
main = do
  runContT (evalStateT runGame initWorld) pure

getAction :: ContT Unit Effect Int
getAction = ContT $ \dispatch -> dispatch 3
