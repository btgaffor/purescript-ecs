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
import Storage (class Has, class PutStore, Entity(..), System, exists, get, getStore, initStore, members, set)
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
    { positions: initStore # insert (Entity 5) (Position 10) # insert (Entity 6) (Position 6)
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

  pure $ p2 + v2

main :: Effect Unit
main = do
  p <- evalStateT runGame initWorld
  log $ show $ p
