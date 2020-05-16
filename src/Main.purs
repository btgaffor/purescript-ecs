module Main where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Reader (ReaderT(..), asks, runReaderT)
import Data.Map as DataMap
import Data.Maybe (Maybe, fromJust)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple2)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)

-- core

type System w a = ReaderT w Effect a

data Entity = Entity Int
derive instance eqEntity :: Eq Entity
derive instance ordEntity :: Ord Entity
instance showEntity :: Show Entity where
  show (Entity value) = show value

-- storage

class F a b | a -> b
instance f1 :: F Int Boolean

f :: forall x. F Int x => x -> String
f n = "hi"
-- a = (f 3)

-- class (Elem (Storage c) ~ c) => Component c where
--   type Storage c

data Map c = Map c

unMap :: forall c. Map c -> c
unMap (Map c) = c

class Storage s

instance mapStorage :: Storage (Map (DataMap.Map Entity c))

class ExplGet s where
  explGet :: forall c. s c -> Entity -> c

instance explGetMap :: ExplGet (DataMap.Map Entity) where
  explGet map entity = unsafePartial (fromJust (DataMap.lookup entity map))

class Functor2 f where
  map :: forall a b. (a -> b) -> f a -> f b

class Has w c where
  getStore :: System w (Map (DataMap.Map Entity c))

-- get shouldn't care what getStore returns as long as it has an instance of ExplGet so we know we can call explGet on it *****************************
-- this will crash if there is no store for that component
get :: forall w c. Has w c => Entity -> System w c
get entity = do
  s <- getStore
  pure $ unsafePartial (fromJust (DataMap.lookup entity (unMap s)))

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
  getStore = asks (\(World world) -> Map world.positions)

instance hasWorldVelocity :: Has World Velocity where
  getStore = asks (\(World world) -> Map world.velocities)

-- instance hasWorldTuple :: Has World (Tuple a b) where
--   getStore = lift2 tuple2 getStore getStore

initWorld :: World
initWorld = World { positions: DataMap.empty # DataMap.insert (Entity 5) (Position 10), velocities: DataMap.empty # DataMap.insert (Entity 5) (Velocity 20) }

runGame :: forall w. Has w Position => Has w Velocity => System w Int
runGame = do
  Position p <- get (Entity 5)
  Velocity v <- get (Entity 5)
  pure $ p + v

main :: Effect Unit
main = do
  p <- runReaderT runGame initWorld
  log $ show $ p
