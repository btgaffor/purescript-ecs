module Main where

import Prelude
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.State (evalStateT, gets, lift, modify_)
import Control.Monad.State as State
import Data.Either (Either(..))
import Data.Map (Map, empty, insert)
import Data.Map as DataMap
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log, logShow)
import Partial.Unsafe (unsafePartial)
import Path (type (/), Choice, Param, S, Anything, parseUrl)
import Storage (class Has, class PutStore, Entity(..), System, get, getStore, initStore, set)
import Type.Prelude (Proxy(..))

data Position
  = Position Int

instance showPosition :: Show Position where
  show (Position num) = show num

data Velocity
  = Velocity Int

instance showVelocity :: Show Velocity where
  show (Velocity num) = show num

data World
  = World
    { positions :: Map Entity Position
    , velocities :: Map Entity Velocity
    }

unWorld ::
  World ->
  { positions :: Map Entity Position
  , velocities :: Map Entity Velocity
  }
unWorld (World world) = world

initWorld :: World
initWorld =
  World
    { positions: initStore # insert (Entity 5) (Position 10)
    , velocities: initStore # insert (Entity 5) (Velocity 5)
    }

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
  pure $ p2 + v2

main :: Effect Unit
main = do
  p <- evalStateT runGame initWorld
  log $ show $ p
