module Storage where

import Prelude
import Control.Apply (lift2)
import Control.Monad.State (StateT)
import Data.Foldable (foldM)
import Data.List (List, filter)
import Data.Map (Map, empty, insert, lookup)
import Data.Map.Internal (keys)
import Data.Maybe (fromJust, isJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Type.Prelude (Proxy(..))

----------
-- Core --
----------
type System w a
  = StateT w Effect a

type SystemT w m a
  = StateT w m a

data Entity
  = Entity Int

data Global c
  = Global c

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

class ExplExists s where
  explExists :: Entity -> s -> Boolean

class ExplMembers s where
  explMembers :: s -> List Entity

class PutStore w s where
  putStore :: s -> System w Unit

class ExplSet s c where
  explSet :: s -> Entity -> c -> s

-------------
-- Systems --
-------------
get :: forall w c s. Has w c s => ExplGet s c => Entity -> System w c
get entity = do
  store <- getStore (Proxy :: Proxy c)
  pure $ explGet entity store

exists :: forall w c s. Has w c s => ExplExists s => Entity -> Proxy c -> System w Boolean
exists entity proxyC = do
  storage <- getStore proxyC
  pure $ explExists entity storage

members :: forall w c s. Has w c s => ExplMembers s => Proxy c -> System w (List Entity)
members proxyC = do
  storage <- getStore proxyC
  pure $ explMembers storage

set :: forall w c s. Has w c s => ExplSet s c => PutStore w s => Entity -> c -> System w Unit
set entity component = do
  store <- getStore (Proxy :: Proxy c)
  newStore <- pure $ explSet store entity component
  putStore newStore

-- TODO: performance measurements on these 2 implementations
cmap ::
  forall w c1 c2 s1 s2.
  Has w c1 s1 =>
  ExplMembers s1 =>
  ExplGet s1 c1 =>
  Has w c2 s2 =>
  ExplSet s2 c2 =>
  PutStore w s2 =>
  (c1 -> c2) -> System w Unit
-- cmap transform = do
--   entities <- members (Proxy :: Proxy c1)
--   foldM
--     ( \accumulator entity -> do
--         old <- get entity
--         set entity (transform old)
--     )
--     unit
--     entities
cmap f = do
  s1 <- getStore (Proxy :: Proxy c1)
  initialS2 <- getStore (Proxy :: Proxy c2)
  let
    entities = explMembers s1
  newS2 <-
    foldM
      ( \store entity ->
          let
            oldComponent = explGet entity s1

            newS2 = explSet store entity (f oldComponent)
          in
            pure newS2
      )
      initialS2
      entities
  putStore newS2

newEntity ::
  forall w s c.
  Has w c s =>
  ExplSet s c =>
  PutStore w s =>
  Has w EntityCount (Global EntityCount) =>
  ExplGet (Global EntityCount) EntityCount =>
  ExplSet (Global EntityCount) EntityCount =>
  PutStore w (Global EntityCount) =>
  c -> System w Entity
newEntity component = do
  EntityCount currentCount <- get (Entity 0)
  storageBefore <- getStore (Proxy :: Proxy c)
  set (Entity currentCount) component
  storageAfter <- getStore (Proxy :: Proxy c)
  set (Entity 0) (EntityCount $ currentCount + 1)
  pure $ Entity currentCount

---------
-- Map --
---------
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
  explSet map entity component = insert entity component map

-----------
-- Tuple --
-----------
instance hasTuple :: (Has w c1 s1, Has w c2 s2) => Has w (Tuple c1 c2) (Tuple s1 s2) where
  getStore _ = lift2 Tuple (getStore (Proxy :: Proxy c1)) (getStore (Proxy :: Proxy c2))

instance explGetTuple :: (ExplGet s1 c1, ExplGet s2 c2) => ExplGet (Tuple s1 s2) (Tuple c1 c2) where
  explGet entity (Tuple storage1 storage2) = Tuple (explGet entity storage1) (explGet entity storage2)

instance explExistsTuple :: (ExplExists s1, ExplExists s2) => ExplExists (Tuple s1 s2) where
  explExists entity (Tuple storage1 storage2) = (explExists entity storage1) && (explExists entity storage2)

instance explMembersTuple :: (ExplMembers s1, ExplExists s2) => ExplMembers (Tuple s1 s2) where
  explMembers (Tuple storage1 storage2) =
    let
      members1 = explMembers storage1
    in
      filter (\entity -> explExists entity storage2) members1

instance putStoreTuple :: (PutStore w s1, PutStore w s2) => PutStore w (Tuple s1 s2) where
  putStore (Tuple storage1 storage2) = putStore storage1 *> putStore storage2

instance explSetTuple :: (ExplSet s1 c1, ExplSet s2 c2) => ExplSet (Tuple s1 s2) (Tuple c1 c2) where
  explSet (Tuple s1 s2) entity (Tuple c1 c2) = Tuple (explSet s1 entity c1) (explSet s2 entity c2)

-------------------
-- EntityCounter --
-------------------
data EntityCount
  = EntityCount Int

-- instance explInitMap :: ExplInit (Map Entity c) where
--   initStore = empty
-- instance explInitEntityCounter :: ExplInit (Global c) where
--   initStore = EntityCounter (EntityCount 0)

instance explInitEntityCounter :: ExplInit (Global EntityCount) where
  initStore = (Global (EntityCount 0))

instance explGetEntityCounter :: ExplGet (Global c) c where
  explGet entity (Global entityCount) = entityCount

instance explExistsEntityCounter :: ExplExists (Global c) where
  explExists entity _ = true

instance explSetEntityCounter :: ExplSet (Global c) c where
  explSet _ _ entityCount = Global entityCount
