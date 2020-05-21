module Main where

import Prelude
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.State (evalStateT, gets)
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
import Storage (class Has, Entity(..), System, get, getStore, initStore)
import Type.Prelude (Proxy(..))

data Position
  = Position Int

data Velocity
  = Velocity Int

data World
  = World
    { positions :: Map Entity Position
    , velocities :: Map Entity Velocity
    }

initWorld :: World
initWorld =
  World
    { positions: initStore # insert (Entity 5) (Position 10)
    , velocities: initStore # insert (Entity 5) (Velocity 5)
    }

instance hasPosition :: Has World Position (Map Entity Position) where
  getStore _ = gets $ \(World world) -> world.positions

instance hasVelocity :: Has World Velocity (Map Entity Velocity) where
  getStore _ = gets $ \(World world) -> world.velocities

runGame :: System World Int
runGame = do
  Position p /\ Velocity v <- get (Entity 5)
  pure $ p + v

main :: Effect Unit
main = do
  p <- evalStateT runGame initWorld
  log $ show $ p
