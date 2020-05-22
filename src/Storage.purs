module Storage where

import Prelude

import Control.Apply (lift2)
import Control.Monad.State (StateT(..), lift)
import Data.Map (Map, empty, insert, lookup, update)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (logShow)
import Partial.Unsafe (unsafePartial)
import Type.Prelude (Proxy(..))

type System w a = StateT w Effect a

data Entity = Entity Int
derive instance eqEntity :: Eq Entity
derive instance ordEntity :: Ord Entity
instance showEntity :: Show Entity where
  show (Entity value) = show value

-- Has

class Has w c s | c -> s where
  getStore :: Proxy c -> System w s

instance hasTuple :: (Has w c1 s1, Has w c2 s2) => Has w (Tuple c1 c2) (Tuple s1 s2) where
  getStore _ =
    let
      storage1 = getStore (Proxy :: Proxy c1)
      storage2 = getStore (Proxy :: Proxy c2)
    in
    lift2 Tuple storage1 storage2

-- Init

class ExplInit s where
  initStore :: s

instance explInitMap :: ExplInit (Map Entity c) where
  initStore = empty

-- Get

class ExplGet s c | s -> c where
  explGet :: Entity -> s -> c

-- | this will crash if there is component for that entity
instance explGetMap :: ExplGet (Map Entity c) c where
  explGet entity map = unsafePartial $ fromJust (lookup entity map)

instance explGetTuple :: (ExplGet s1 c1, ExplGet s2 c2) => ExplGet (Tuple s1 s2) (Tuple c1 c2) where
  explGet entity (Tuple storage1 storage2) = Tuple (explGet entity storage1) (explGet entity storage2)

get :: forall w c s. Has w c s => ExplGet s c => Entity -> System w c
get entity = do
  store <- getStore (Proxy :: Proxy c)
  pure $ explGet entity store

-- Set

class PutStore w s where
  putStore :: s -> System w Unit

instance putTuple :: (PutStore w s1, PutStore w s2) => PutStore w (Tuple s1 s2) where
  putStore (Tuple storage1 storage2) = do
    putStore storage1
    putStore storage2
    pure unit

class ExplSet s c where
  explSet :: s -> Entity -> c -> s

instance explSetMap :: ExplSet (Map Entity c) c where
  explSet map entity component = update (\_ -> Just component) entity map

instance explSetTuple :: (ExplSet s1 c1, ExplSet s2 c2) => ExplSet (Tuple s1 s2) (Tuple c1 c2) where
  explSet (Tuple s1 s2) entity (Tuple c1 c2) =
    let
      newS1 = explSet s1 entity c1
      newS2 = explSet s2 entity c2
    in
    Tuple newS1 newS2

set :: forall w c s. Show s => Has w c s => ExplSet s c => PutStore w s => Entity -> c -> System w Unit
set entity component = do
  store <- getStore (Proxy :: Proxy c)
  newStore <- pure $ explSet store entity component
  putStore newStore
