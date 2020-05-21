module World where

import Prelude

import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Either (Either(..))
import Data.Map as DataMap
import Data.Maybe (fromJust)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Path (type (/), Choice, Param, S, Anything, parseUrl)
import Type.Prelude (Proxy(..))

-- core

type System w a = ReaderT w Effect a

data Entity = Entity Int
derive instance eqEntity :: Eq Entity
derive instance ordEntity :: Ord Entity
instance showEntity :: Show Entity where
  show (Entity value) = show value

-- storage

class Has w c where
  getStore :: System w (DataMap.Map Entity c)

-- this will crash if there is component for that entity
get :: forall w c. Has w c => Entity -> System w c
get entity = do
  s <- getStore
  pure $ unsafePartial (fromJust (DataMap.lookup entity s))

-- components

data Position = Position Int
instance showPosition :: Show Position where
  show (Position value) = show value

data Velocity = Velocity Int
instance showVelocity :: Show Velocity where
  show (Velocity value) = show value

-- world

data World = World { positions :: DataMap.Map Entity Position, velocities :: DataMap.Map Entity Velocity }
instance showWorld :: Show World where
  show (World world) = show world

instance hasWorldPosition :: Has World Position where
  getStore = asks (\(World world) -> world.positions)

instance hasWorldVelocity :: Has World Velocity where
  getStore = asks (\(World world) -> world.velocities)

-- instance hasWorldTuple :: Has World (Tuple a b) where
--   getStore = lift2 tuple2 getStore getStore

data Map c = Map (DataMap.Map Entity c)

initWorld :: World
initWorld = World { positions: DataMap.empty # DataMap.insert (Entity 5) (Position 10), velocities: DataMap.empty # DataMap.insert (Entity 5) (Velocity 20) }

runGame :: System World Int
runGame = do
  -- s :: DataMap.Map Entity Position <- getStore
  -- let
  --   Position p2 = unsafePartial (fromJust (DataMap.lookup (Entity 5) s))
  Position p <- get (Entity 5)
  Velocity v <- get (Entity 5)
  pure $ p + v
