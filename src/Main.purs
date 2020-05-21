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
import Storage (class Has, Entity(..), System, get, getStore)
import Type.Prelude (Proxy(..))
import World (initWorld, runGame)

data Position = Position Int
data Velocity = Velocity Int

data World = World { positions :: Map Int Position, velocities :: Map Int Velocity }

instance hasPosition :: Has World Position (Map Int Position) where
  getStore _ _ = gets (\(World world) -> world.positions)

instance hasVelocity :: Has World Velocity (Map Int Velocity) where
  getStore _ _ = gets (\(World world) -> world.velocities)

test :: System World Int
test = do
  Position p /\ Velocity v <- get (Entity 5)
  pure $ p + v

main :: Effect Unit
main = do
  p <- evalStateT test (World { positions: empty # insert 5 (Position 10), velocities: empty # insert 5 (Velocity 5) })
  log $ show $ p
