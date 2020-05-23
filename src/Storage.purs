module Storage where

import Prelude

import Control.Apply (lift2)
import Control.Monad.State (StateT(..), lift)
import Data.List (List, filter)
import Data.Map (Map, empty, insert, lookup, update, values)
import Data.Map.Internal (keys)
import Data.Maybe (Maybe(..), fromJust, isJust)
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

class Has w c s | c -> s where
  getStore :: Proxy c -> System w s

class ExplInit s where
  initStore :: s

class ExplGet s c | s -> c where
  explGet :: Entity -> s -> c

get :: forall w c s. Has w c s => ExplGet s c => Entity -> System w c
get entity = do
  store <- getStore (Proxy :: Proxy c)
  pure $ explGet entity store

class ExplExists s where
  explExists :: Entity -> s -> Boolean

exists :: forall w c s. Has w c s => ExplExists s => Entity -> Proxy c -> System w Boolean
exists entity proxyC = do
  storage <- getStore proxyC
  pure $ explExists entity storage

class ExplMembers s where
  explMembers :: s -> List Entity

members :: forall w c s. Has w c s => ExplMembers s => Proxy c -> System w (List Entity)
members proxyC = do
  storage <- getStore proxyC
  pure $ explMembers storage

class PutStore w s where
  putStore :: s -> System w Unit

class ExplSet s c where
  explSet :: s -> Entity -> c -> s

set :: forall w c s. Show s => Has w c s => ExplSet s c => PutStore w s => Entity -> c -> System w Unit
set entity component = do
  store <- getStore (Proxy :: Proxy c)
  newStore <- pure $ explSet store entity component
  putStore newStore

-- Map

instance explInitMap :: ExplInit (Map Entity c) where
  initStore = empty

  -- | this will crash if there is component for that entity
instance explGetMap :: ExplGet (Map Entity c) c where
  explGet entity map = unsafePartial $ fromJust (lookup entity map)

instance explExistsMap :: ExplExists (Map Entity c) where
  explExists entity map = isJust (lookup entity map)

instance explMembersMap :: ExplMembers (Map Entity c) where
  explMembers map = keys map

instance explSetMap :: ExplSet (Map Entity c) c where
  explSet map entity component = update (\_ -> Just component) entity map

-- Tuple

instance hasTuple :: (Has w c1 s1, Has w c2 s2) => Has w (Tuple c1 c2) (Tuple s1 s2) where
  getStore _ =
     lift2 Tuple (getStore (Proxy :: Proxy c1)) (getStore (Proxy :: Proxy c2))

instance explGetTuple :: (ExplGet s1 c1, ExplGet s2 c2) => ExplGet (Tuple s1 s2) (Tuple c1 c2) where
  explGet entity (Tuple storage1 storage2) =
    Tuple (explGet entity storage1) (explGet entity storage2)

instance explExistsTuple :: (ExplExists s1, ExplExists s2) => ExplExists (Tuple s1 s2) where
  explExists entity (Tuple storage1 storage2) =
    (explExists entity storage1) && (explExists entity storage2)

instance explMembersTuple :: (ExplMembers s1, ExplExists s2) => ExplMembers (Tuple s1 s2) where
  explMembers (Tuple storage1 storage2) =
    let
      members1 = explMembers storage1
    in
    filter (\entity -> explExists entity storage2) members1

instance putStoreTuple :: (PutStore w s1, PutStore w s2) => PutStore w (Tuple s1 s2) where
  putStore (Tuple storage1 storage2) =
    putStore storage1 *> putStore storage2

instance explSetTuple :: (ExplSet s1 c1, ExplSet s2 c2) => ExplSet (Tuple s1 s2) (Tuple c1 c2) where
  explSet (Tuple s1 s2) entity (Tuple c1 c2) =
     Tuple (explSet s1 entity c1) (explSet s2 entity c2)
