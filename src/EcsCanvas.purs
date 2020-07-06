module EcsCanvas where

import Prelude
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Effect (Effect)
import Graphics.Canvas (Arc, Context2D, Rectangle, arc, beginPath, closePath, fill, fillPath, fillText, lineTo, moveTo, rect, setFillStyle, setFont, withContext, bezierCurveTo) as Canvas
import Graphics.Canvas (BezierCurve)

withContext :: ReaderT Canvas.Context2D Effect Unit -> ReaderT Canvas.Context2D Effect Unit
withContext action = do
  ctx <- ask
  lift $ Canvas.withContext ctx (runReaderT action ctx)

fillPath :: ReaderT Canvas.Context2D Effect Unit -> ReaderT Canvas.Context2D Effect Unit
fillPath action = do
  ctx <- ask
  lift $ Canvas.fillPath ctx (runReaderT action ctx)

setFillStyle :: String -> ReaderT Canvas.Context2D Effect Unit
setFillStyle style = do
  ctx <- ask
  lift $ Canvas.setFillStyle ctx style

setFont :: String -> ReaderT Canvas.Context2D Effect Unit
setFont style = do
  ctx <- ask
  lift $ Canvas.setFont ctx style

fillText :: String -> Number -> Number -> ReaderT Canvas.Context2D Effect Unit
fillText value x y = do
  ctx <- ask
  lift $ Canvas.fillText ctx value x y

beginPath :: ReaderT Canvas.Context2D Effect Unit
beginPath = do
  ctx <- ask
  lift $ Canvas.beginPath ctx

moveTo :: Number -> Number -> ReaderT Canvas.Context2D Effect Unit
moveTo x y = do
  ctx <- ask
  lift $ Canvas.moveTo ctx x y

lineTo :: Number -> Number -> ReaderT Canvas.Context2D Effect Unit
lineTo x y = do
  ctx <- ask
  lift $ Canvas.lineTo ctx x y

closePath :: ReaderT Canvas.Context2D Effect Unit
closePath = do
  ctx <- ask
  lift $ Canvas.closePath ctx

arc :: Canvas.Arc -> ReaderT Canvas.Context2D Effect Unit
arc arcPath = do
  ctx <- ask
  lift $ Canvas.arc ctx arcPath

rect :: Canvas.Rectangle -> ReaderT Canvas.Context2D Effect Unit
rect rectPath = do
  ctx <- ask
  lift $ Canvas.rect ctx rectPath

fill :: ReaderT Canvas.Context2D Effect Unit
fill = do
  ctx <- ask
  lift $ Canvas.fill ctx

bezierCurveTo :: BezierCurve -> ReaderT Canvas.Context2D Effect Unit
bezierCurveTo curve = do
  ctx <- ask
  lift $ Canvas.bezierCurveTo ctx curve

renderEllipse :: Number -> Number -> Number -> Number -> ReaderT Canvas.Context2D Effect Unit
renderEllipse x y width height = do
  ctx <- ask
  let
    widthOver2 = width / 2.0

    widthTwoThirds = width * 2.0 / 3.0

    heightOver2 = height / 2.0
  fillPath do
    moveTo x (y - heightOver2)
    bezierCurveTo
      { cp1x: x + widthTwoThirds
      , cp1y: y - heightOver2
      , cp2x: x + widthTwoThirds
      , cp2y: y + heightOver2
      , x
      , y: y + heightOver2
      }
    bezierCurveTo
      { cp1x: x - widthTwoThirds
      , cp1y: y + heightOver2
      , cp2x: x - widthTwoThirds
      , cp2y: y - heightOver2
      , x
      , y: y - heightOver2
      }
    closePath
