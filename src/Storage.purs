module Storage where

import Prelude

import Control.Apply (lift2)
import Control.Monad.State (StateT(..))
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
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

instance hasTuple :: (Has w c1 s1, Has w c2 s2) => Has w (Tuple c1 c2) (Tuple s1 s2) where
  getStore _ =
    let
      storage1 = getStore (Proxy :: Proxy c1)
      storage2 = getStore (Proxy :: Proxy c2)
    in
    lift2 Tuple storage1 storage2

class ExplInit s where
  initStore :: s

instance explInitMap :: ExplInit (Map Entity c) where
  initStore = empty

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
