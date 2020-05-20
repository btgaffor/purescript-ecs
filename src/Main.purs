module Main where

import Prelude

import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Either (Either(..))
import Data.Map as DataMap
import Data.Maybe (fromJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Partial.Unsafe (unsafePartial)
import Path (type (/), Choice, Param, S, Anything, parseUrl)
import Storage (Position(..), Velocity(..), blah, get)
import Type.Prelude (Proxy(..))

-- main :: Effect Unit
-- main = do
--   p <- runReaderT runGame initWorld
--   log $ show $ p

-- myRouteP :: Proxy MyRoute
-- myRouteP = Proxy

-- main :: Effect Unit
-- main = do
--   case (parseUrl myRouteP "hello/world") of
--     Left err -> log "error"
--     Right obj -> log $ show obj

type MyRoute = S "hello" / Anything / Param "id" Int / Param "name" String / Param "choice" Choice

testUrl :: String
testUrl = "/hello/wod/1/joe/B"

-- main :: Effect Unit
-- main = do
--   case parseUrl (Proxy :: Proxy MyRoute) testUrl of
--     Left e -> log $ "oops: " <> show e
--     Right result ->
--       log $ show result

main :: Effect Unit
main = logShow blah
