module Ecs where

import Prelude

import Control.Apply (lift2)
import Control.Monad.State (StateT)
import Data.Either (Either(..))
import Data.Foldable (foldM, foldl)
import Data.List (List(..), filter, singleton)
import Data.Map (Map, delete, empty, insert, lookup)
import Data.Map.Internal (keys)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Type.Prelude (Proxy(..))

----------------------
-- Data Definitions --
----------------------
data Entity
  = Entity Int

instance showEntity :: Show Entity where
  show (Entity value) = "(Entity " <> show value <> ")"

derive instance eqEntity :: Eq Entity

derive instance ordEntity :: Ord Entity

type System w a
  = StateT w Effect a

type SystemT w m a
  = StateT w m a

data Global c
  = Global c

data Unique c
  = Present Entity c | Absent

----------------------
-- Core Typeclasses --
----------------------
class GetStore w c s | c -> s where
  getStore :: forall m. Monad m => Proxy c -> SystemT w m s

class ExplInit s where
  initStore :: s

class ExplGet s c | s -> c where
  explGet :: Entity -> s -> c
  explExists :: Entity -> s -> Boolean

class ExplMembers s where
  explMembers :: s -> List Entity

class SaveStore w s where
  saveStore :: forall m. Monad m => s -> SystemT w m Unit

class ExplSet s c where
  explSet :: s -> Entity -> c -> s

class ExplDestroy s where
  explDestroy :: s -> Entity -> s

-------------
-- Systems --
-------------
get ::
  forall w c s m.
  Monad m =>
  GetStore w c s =>
  ExplGet s c =>
  Entity -> SystemT w m c
get entity = do
  store <- getStore (Proxy :: Proxy c)
  pure $ explGet entity store

exists ::
  forall w c s m.
  Monad m =>
  GetStore w c s =>
  ExplGet s c =>
  Entity -> Proxy c -> SystemT w m Boolean
exists entity proxyC = do
  storage <- getStore proxyC
  pure $ explExists entity storage

members ::
  forall w c s m.
  Monad m =>
  GetStore w c s =>
  ExplMembers s =>
  Proxy c -> SystemT w m (List Entity)
members proxyC = do
  storage <- getStore proxyC
  pure $ explMembers storage

set ::
  forall w c s m.
  Monad m =>
  GetStore w c s => SaveStore w s =>
  ExplSet s c =>
  Entity -> c -> SystemT w m Unit
set entity component = do
  store <- getStore (Proxy :: Proxy c)
  saveStore (explSet store entity component)

destroy ::
  forall w c s m.
  Monad m =>
  GetStore w c s => SaveStore w s =>
  ExplDestroy s =>
  Entity -> Proxy c -> SystemT w m Unit
destroy entity proxyC = do
  store <- getStore proxyC
  saveStore (explDestroy store entity)

-- | Similar to cmap, but for global components. cmap can't be used because
--   global stores don't have an ExplMembers instance.
modifyGlobal ::
  forall w c s m.
  Monad m =>
  GetStore w c s => SaveStore w s =>
  ExplGet s c => ExplSet s c =>
  (c -> c) -> SystemT w m Unit
modifyGlobal transform = do
  component <- get (Entity 0)
  set (Entity 0) (transform component)

cmap ::
  forall w c1 c2 s1 s2 m.
  Monad m =>
  GetStore w c1 s1 =>
  GetStore w c2 s2 => SaveStore w s2 =>
  ExplMembers s1 =>
  ExplGet s1 c1 =>
  ExplSet s2 c2 =>
  (c1 -> c2) -> SystemT w m Unit
cmap transform = do
  entities <- members (Proxy :: Proxy c1)
  foldM
    ( \accumulator entity -> do
        old <- get entity
        set entity (transform old)
    )
    unit
    entities

cmapM ::
  forall w c1 c2 s1 s2 m.
  Monad m =>
  GetStore w c1 s1 =>
  ExplMembers s1 =>
  ExplGet s1 c1 =>
  GetStore w c2 s2 =>
  ExplSet s2 c2 =>
  SaveStore w s2 =>
  (c1 -> SystemT w m c2) -> SystemT w m Unit
cmapM transform = do
  entities <- members (Proxy :: Proxy c1)
  foldM
    ( \accumulator entity -> do
        old <- get entity
        new <- transform old
        set entity new
    )
    unit
    entities

cmapM_ ::
  forall w c1 c2 s1 s2 m.
  Monad m =>
  GetStore w c1 s1 =>
  ExplMembers s1 =>
  ExplGet s1 c1 =>
  GetStore w c2 s2 =>
  ExplSet s2 c2 =>
  SaveStore w s2 =>
  (c1 -> SystemT w m c2) -> SystemT w m Unit
cmapM_ transform = do
  entities <- members (Proxy :: Proxy c1)
  foldM
    ( \accumulator entity -> do
        old <- get entity
        void $ transform old
    )
    unit
    entities

cfold :: forall w m s c a. Monad m => GetStore w c s => ExplMembers s => ExplGet s c => (a -> c -> a) -> a -> SystemT w m a
cfold combiner accumulator0 = do
  store <- getStore (Proxy :: Proxy c)
  let entities = explMembers store
  pure $ foldl (\accumulator entity -> combiner accumulator (explGet entity store)) accumulator0 entities

newEntity ::
  forall w s c m.
  Monad m =>
  GetStore w c s =>
  ExplSet s c =>
  SaveStore w s =>
  GetStore w EntityCount (Global EntityCount) =>
  ExplGet (Global EntityCount) EntityCount =>
  ExplSet (Global EntityCount) EntityCount =>
  SaveStore w (Global EntityCount) =>
  c -> SystemT w m Entity
newEntity component = do
  EntityCount currentCount <- get (Entity 0)
  set (Entity currentCount) component
  set (Entity 0) (EntityCount $ currentCount + 1)
  pure $ Entity currentCount

----------
-- Unit --
----------
instance getStoreUnit :: GetStore w Unit Unit where
  getStore _ = pure unit

-- | Useful for when you don't want to set anything
instance explSetUnit :: ExplSet Unit Unit where
  explSet s entity component = unit

instance saveStoreUnit :: SaveStore w Unit where
  saveStore _ = pure unit

-----------------
-- EntityStore --
-----------------
data EntityStore = EntityStore

instance getStoreEntity :: GetStore w Entity EntityStore where
  getStore _ = pure EntityStore

instance explGetEntity :: ExplGet EntityStore Entity where
  explGet entity _ = entity
  explExists _ _ = true

instance saveStoreEntity :: SaveStore w EntityStore where
  saveStore _ = pure unit

---------
-- Map --
---------
instance explInitMap :: ExplInit (Map Entity c) where
  initStore = empty

-- | this will crash if there is no component for that entity
instance explGetMap :: ExplGet (Map Entity c) c where
  explGet entity map = unsafePartial $ fromJust (lookup entity map)
  explExists entity map = isJust (lookup entity map)

instance explMembersMap :: ExplMembers (Map Entity c) where
  explMembers map = keys map

instance explSetMap :: ExplSet (Map Entity c) c where
  explSet map entity component = insert entity component map

instance explDestroyMap :: ExplDestroy (Map Entity c) where
  explDestroy map entity = delete entity map

-----------
-- Tuple --
-----------
instance hasTuple :: (GetStore w c1 s1, GetStore w c2 s2) => GetStore w (Tuple c1 c2) (Tuple s1 s2) where
  getStore _ = lift2 Tuple (getStore (Proxy :: Proxy c1)) (getStore (Proxy :: Proxy c2))

instance explGetTuple :: (ExplGet s1 c1, ExplGet s2 c2) => ExplGet (Tuple s1 s2) (Tuple c1 c2) where
  explGet entity (Tuple storage1 storage2) = Tuple (explGet entity storage1) (explGet entity storage2)
  explExists entity (Tuple storage1 storage2) = (explExists entity storage1) && (explExists entity storage2)

instance explMembersTuple :: (ExplMembers s1, ExplGet s2 c2) => ExplMembers (Tuple s1 s2) where
  explMembers (Tuple storage1 storage2) =
    let
      members1 = explMembers storage1
    in
      filter (\entity -> explExists entity storage2) members1

instance saveStoreTuple :: (SaveStore w s1, SaveStore w s2) => SaveStore w (Tuple s1 s2) where
  saveStore (Tuple storage1 storage2) = saveStore storage1 *> saveStore storage2

instance explSetTuple :: (ExplSet s1 c1, ExplSet s2 c2) => ExplSet (Tuple s1 s2) (Tuple c1 c2) where
  explSet (Tuple s1 s2) entity (Tuple c1 c2) = Tuple (explSet s1 entity c1) (explSet s2 entity c2)

instance explDestroyTuple :: (ExplDestroy s1, ExplDestroy s2) => ExplDestroy (Tuple s1 s2) where
  explDestroy (Tuple s1 s2) entity = Tuple (explDestroy s1 entity) (explDestroy s2 entity)

------------
-- Either --
------------
data EitherStore s1 s2 = EitherStore s1 s2

instance hasEither :: (GetStore w c1 s1, GetStore w c2 s2) => GetStore w (Either c1 c2) (EitherStore s1 s2) where
  getStore _ = EitherStore <$> getStore (Proxy :: Proxy c1) <*> getStore (Proxy :: Proxy c2)

instance explGetEither :: (ExplGet s1 c1, ExplGet s2 c2) => ExplGet (EitherStore s1 s2) (Either c1 c2) where
  explGet entity (EitherStore store1 store2) =
    if (explExists entity store2)
       then Right $ explGet entity store2
       else Left $ explGet entity store1

  explExists entity (EitherStore store1 store2) =
    if (explExists entity store2)
       then true
       else explExists entity store1

instance saveStoreEither :: (SaveStore w s1, SaveStore w s2) => SaveStore w (EitherStore s1 s2) where
  saveStore (EitherStore storage1 storage2) = saveStore storage1 *> saveStore storage2

instance explSetEither :: (ExplSet s1 c1, ExplSet s2 c2) => ExplSet (EitherStore s1 s2) (Either c1 c2) where
  explSet (EitherStore s1 s2) entity component = case component of
    Left component' -> EitherStore (explSet s1 entity component') s2
    Right component' -> EitherStore s1 (explSet s2 entity component')

instance explDestroyEither :: (ExplDestroy s1, ExplDestroy s2) => ExplDestroy (EitherStore s1 s2) where
  explDestroy (EitherStore store1 store2) entity =
    EitherStore (explDestroy store1 entity) (explDestroy store2 entity)

------------
-- Global --
------------
instance explGetGlobal :: ExplGet (Global c) c where
  explGet entity (Global c) = c
  explExists entity _ = true

instance explSetGlobal :: ExplSet (Global c) c where
  explSet _ _ c = Global c

------------
-- Unique --
------------
instance explInitUnique :: ExplInit (Unique c) where
  initStore = Absent

-- | this will error if the entity isn't set
instance explGetUnique :: ExplGet (Unique c) c where
  explGet entity c = unsafePartial $ case c of
    Present e c' -> c'
  explExists entity c = case c of
    Absent -> false
    Present e _ -> e == entity

instance explMembersUnique :: ExplMembers (Unique c) where
  explMembers c = case c of
    Absent -> Nil
    Present e _ -> singleton(e)

instance explSetUnique :: ExplSet (Unique c) c where
  explSet _ e c = Present e c

instance explDestroyUnique :: ExplDestroy (Unique c) where
  explDestroy _ _ = Absent

-------------------
-- EntityCounter --
-------------------
data EntityCount
  = EntityCount Int

instance explInitEntityCounter :: ExplInit (Global EntityCount) where
  initStore = (Global (EntityCount 0))

---------
-- Not --
---------
data Not c = Not
data NotStore s = NotStore s

instance hasNot :: GetStore w c s => GetStore w (Not c) (NotStore s) where
  getStore _ = NotStore <$> (getStore (Proxy :: Proxy c))

instance explGetNot :: ExplGet s c => ExplGet (NotStore s) (Not c) where
  explGet _ _ = Not
  explExists entity (NotStore s) = not (explExists entity s)

instance saveStoreNot :: SaveStore w s => SaveStore w (NotStore s) where
  saveStore (NotStore s) = saveStore s

instance explSetNot :: ExplDestroy s => ExplSet (NotStore s) (Not c) where
  explSet (NotStore s) entity _ = NotStore (explDestroy s entity)

-----------
-- Maybe --
-----------
data MaybeStore s = MaybeStore s

instance hasMaybe :: GetStore w c s => GetStore w (Maybe c) (MaybeStore s) where
  getStore _ = MaybeStore <$> (getStore (Proxy :: Proxy c))

instance explGetMaybe :: ExplGet s c => ExplGet (MaybeStore s) (Maybe c) where
  explGet entity (MaybeStore store) = if explExists entity store then Just (explGet entity store) else Nothing
  explExists _ _ = true

instance saveStoreMaybe :: SaveStore w s => SaveStore w (MaybeStore s) where
  saveStore (MaybeStore s) = saveStore s

instance explSetMaybe :: (ExplSet s c, ExplDestroy s) => ExplSet (MaybeStore s) (Maybe c) where
  explSet (MaybeStore s) entity component = case component of
    Just component' -> MaybeStore $ explSet s entity component'
    Nothing -> MaybeStore $ explDestroy s entity
