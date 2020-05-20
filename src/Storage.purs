module Storage where

import Prelude

import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Type.Prelude (Proxy(..))
import Data.Tuple.Nested ((/\))

-- | another type operator alias for Tuple /\ for familiarity convenience
-- infixr 6 type Tuple as ,

data Position = Position Int
data Velocity = Velocity Int
data Container v = Container v

class Has c s | c -> s where
  getStore :: Proxy c -> s

instance hasPosition :: Has Position (Map Int Position) where
  getStore _ = empty # insert 5 (Position 10)

instance hasVelocity :: Has Velocity (Map Int Velocity) where
  getStore _ = empty # insert 5 (Velocity 5)

instance hasContainer :: Has c s => Has (Container c) (Container s) where
  getStore _ =
    let
      store = getStore (Proxy :: Proxy c)
    in
     Container store

instance hasTuple :: (Has c1 s1, Has c2 s2) => Has (Tuple c1 c2) (Tuple s1 s2) where
  getStore _ =
    let
      storage1 = getStore (Proxy :: Proxy c1)
      storage2 = getStore (Proxy :: Proxy c2)
    in
    Tuple storage1 storage2

class ExplGet s c | s -> c where
  explGet :: Int -> s -> c

instance explGetMap :: ExplGet (Map Int c) c where
  explGet entity map = unsafePartial $ fromJust (lookup entity map)

instance explGetTuple :: (ExplGet s1 c1, ExplGet s2 c2) => ExplGet (Tuple s1 s2) (Tuple c1 c2) where
  explGet entity (Tuple storage1 storage2) = Tuple (explGet entity storage1) (explGet entity storage2)

instance explGetContainer :: ExplGet c s => ExplGet (Container c) (Container s) where
  explGet entity (Container storage) = Container $ explGet entity storage

get :: forall c s. Has c s => ExplGet s c => Int -> c
get entity =
  let
    store = getStore (Proxy :: Proxy c)
  in
  explGet entity store

blah :: Int
blah =
  let
    Container (Position p0) = get 5
    (Position p) /\ (Velocity v) = get 5
  in
  p0 + p + v

-- get :: forall s c. Has c s => c
-- get = ?wut
