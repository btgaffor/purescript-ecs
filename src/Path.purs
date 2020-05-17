module Path where

import Prelude
import Control.Monad.Except (Except, runExcept, throwError)
import Data.Either (Either)
import Data.Int (decimal, fromNumber, toStringAs)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(Pattern), drop, indexOf, splitAt, stripPrefix)
import Data.Tuple (Tuple)
import Global (readInt)
import Prim.Row (class Cons)
import Record (get)
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Prelude (class IsSymbol, Proxy(..), SProxy(..), reflectSymbol)
import Type.Row (class Lacks)

parseUrl ::
  forall to xs.
  ParseURLImpl xs () to =>
  Proxy xs ->
  String ->
  Either BadTimes (Record to)
parseUrl p s =
  runExcept do
    result <- parseURLImpl p s
    pure $ Builder.build result.builder {}

writeUrl ::
  forall row xs.
  WriteURLImpl xs row =>
  Proxy xs ->
  { | row } ->
  String
writeUrl = writeURLImpl

data BadTime
  = SymbolMatchError String
  | ParamParseError String

derive instance badTimeEq :: Eq BadTime

instance badTimeShow :: Show BadTime where
  show (SymbolMatchError s) = "SymbolMatchError " <> s
  show (ParamParseError s) = "ParamParseError " <> s

type BadTimes
  = NonEmptyList BadTime

-- | a URL Parameter argument, by the label that should be used in the params record
-- | and type that this param should be parsed to.
data Param (label :: Symbol) ty
  = Param

-- | another type operator alias for Tuple /\ for familiarity convenience
infixr 6 type Tuple as /

-- | shorthand for SProxy
type S
  = SProxy

-- | Typeclass for reading from URL segments
class ParseParam a where
  parseParam :: String -> Except BadTimes a

instance stringParseParam :: ParseParam String where
  parseParam s = pure s

instance intParseParam :: ParseParam Int where
  parseParam s = case fromNumber $ readInt 10 s of
    Just a -> pure a
    Nothing ->
      throwError <<< pure <<< ParamParseError
        $ "could not parse "
        <> s
        <> " into integer"

-- | Typeclass to parse a URL segment into a Record.Builder that extracts out parameters
class ParseURLImpl xs (from :: #Type) (to :: #Type) | xs -> from to where
  parseURLImpl ::
    Proxy xs ->
    String ->
    Except BadTimes
      { builder :: Builder (Record from) (Record to)
      , remaining :: String
      }

instance tupleParseURL ::
  ( ParseURLImpl left from' to
  , ParseURLImpl right from from'
  ) =>
  ParseURLImpl (left / right) from to where
  parseURLImpl _ s = do
    left <- parseURLImpl (Proxy :: Proxy left) s
    right <- parseURLImpl (Proxy :: Proxy right) left.remaining
    pure $ { builder: left.builder <<< right.builder, remaining: right.remaining }

instance segmentParseURL ::
  ( IsSymbol segment
    ) =>
  ParseURLImpl (SProxy segment) from from where
  parseURLImpl _ s = case stripPrefix (Pattern $ "/" <> segment) s of
    Nothing ->
      throwError <<< pure <<< SymbolMatchError
        $ "could not strip segment "
        <> segment
        <> " from path "
        <> s
    Just remaining -> pure { builder: identity, remaining }
    where
    segment = reflectSymbol (SProxy :: SProxy segment)

instance paramParseURL ::
  ( IsSymbol label
  , ParseParam ty
  , Lacks label from
  , Cons label ty from to
  ) =>
  ParseURLImpl (Param label ty) from to where
  parseURLImpl _ s = do
    split' <-
      maybe
        ( throwError <<< pure <<< ParamParseError
            $ "could not handle url param segment "
            <> s
        )
        pure
        split
    value <- parseParam split'.before
    pure { builder: Builder.insert labelP value, remaining: split'.after }
    where
    labelP = SProxy :: SProxy label

    label = reflectSymbol labelP

    s' = drop 1 s

    split = case indexOf (Pattern "/") s' of
      Just idx -> pure $ splitAt idx s'
      Nothing -> pure { before: s', after: "" }

-- | Typeclass for writing URL segments
class WriteParam a where
  writeParam :: a -> String

instance stringWriteParam :: WriteParam String where
  writeParam s = s

instance intWriteParam :: WriteParam Int where
  writeParam i = toStringAs decimal i

-- | Typeclass to write URL segments from records for params
class WriteURLImpl xs (row :: #Type) | xs -> row where
  writeURLImpl ::
    Proxy xs ->
    Record row ->
    String

instance tupleWriteURL ::
  ( WriteURLImpl left row
  , WriteURLImpl right row
  ) =>
  WriteURLImpl (left / right) row where
  writeURLImpl _ r = left <> right
    where
    left = writeURLImpl (Proxy :: Proxy left) r

    right = writeURLImpl (Proxy :: Proxy right) r

instance segmentWriteURL ::
  ( IsSymbol segment
    ) =>
  WriteURLImpl (SProxy segment) row where
  writeURLImpl _ _ = "/" <> reflectSymbol (SProxy :: SProxy segment)

instance paramWriteURL ::
  ( IsSymbol label
  , Cons label ty trash row
  , WriteParam ty
  ) =>
  WriteURLImpl (Param label ty) row where
  writeURLImpl _ r = "/" <> param
    where
    x = get (SProxy :: SProxy label) r

    param = writeParam x
