module EcsCanvas where

import Prelude

import Control.Monad.Cont (ContT(..), runContT)
import Control.Monad.State (evalStateT, lift)
import Data.Maybe (Maybe(..))
import Ecs (SystemT)
import Effect (Effect)
import Effect.Console (error, log)
import Effect.Ref (Ref, modify_, new, read)
import Graphics.Canvas (Context2D, clearRect, getCanvasElementById, getContext2D)
import Web.DOM.Node (toEventTarget)
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window) as HTML
import Web.HTML.HTMLDocument (body) as HTML
import Web.HTML.HTMLElement as HTML.HTMLElement
import Web.HTML.Window as HTML.Window
import Web.UIEvent.KeyboardEvent as KeyboardEvent

canvasWidth :: Number
canvasWidth = 800.0

canvasHeight :: Number
canvasHeight = 600.0

type Keys
  = { arrowLeft :: Boolean
    , arrowRight :: Boolean
    , space :: Boolean
    }
initKeys :: Keys
initKeys
  = { arrowLeft: false
    , arrowRight: false
    , space: false
    }

handleKeydown :: Ref Keys -> Event -> Effect Unit
handleKeydown keysRef event = case KeyboardEvent.fromEvent event of
  Nothing -> pure unit
  -- Just keyboardEvent -> log $ "keydown: " <> KeyboardEvent.code keyboardEvent
  Just keyboardEvent -> case KeyboardEvent.code keyboardEvent of
    "ArrowLeft" -> modify_ (\s -> s { arrowLeft = true }) keysRef
    "ArrowRight" -> modify_ (\s -> s { arrowRight = true }) keysRef
    "Space" -> modify_ (\s -> s { space = true }) keysRef
    code -> log code

handleKeyup :: Ref Keys -> Event -> Effect Unit
handleKeyup keysRef event = case KeyboardEvent.fromEvent event of
  Nothing -> pure unit
  Just keyboardEvent -> case KeyboardEvent.code keyboardEvent of
    "ArrowLeft" -> modify_ (\s -> s { arrowLeft = false }) keysRef
    "ArrowRight" -> modify_ (\s -> s { arrowRight = false }) keysRef
    "Space" -> modify_ (\s -> s { space = false }) keysRef
    _ -> pure unit

setupInput :: Ref Keys -> Effect Unit
setupInput keysRef = do
  keydownListener <- eventListener (handleKeydown keysRef)
  keyupListener <- eventListener (handleKeyup keysRef)
  mBody <- HTML.body =<< HTML.Window.document =<< HTML.window
  case mBody of
    Nothing -> error "no body element to attach event listener to"
    Just body' -> do
      addEventListener (EventType "keydown") keydownListener false (toEventTarget $ HTML.HTMLElement.toNode body')
      addEventListener (EventType "keyup") keyupListener false (toEventTarget $ HTML.HTMLElement.toNode body')

type GameSetup w
  = SystemT w (ContT Unit Effect) Unit

type StepFrameKeys w
  = Keys -> SystemT w (ContT Unit Effect) Unit

type StepFrame w
  = SystemT w (ContT Unit Effect) Unit

type RenderFrame w
  = SystemT w (ContT Unit Effect) (Context2D -> Effect Unit)

runGame :: forall w. GameSetup w -> StepFrameKeys w -> RenderFrame w -> SystemT w (ContT Unit Effect) Unit
runGame gameSetup gameFrame renderFrame = do
  keysRef <- lift <<< lift $ new initKeys
  lift <<< lift $ setupInput keysRef
  gameSetup
  gameLoop keysRef gameFrame renderFrame

gameLoop :: forall w. Ref Keys -> StepFrameKeys w -> RenderFrame w -> SystemT w (ContT Unit Effect) Unit
gameLoop keysRef gameFrame renderFrame = loop
  where
  loop = do
    keys <- lift <<< lift $ read keysRef
    gameFrame keys
    renderer <- renderFrame
    w <- lift <<< lift $ HTML.window
    lift $ ContT $ \next -> void $ HTML.Window.requestAnimationFrame (rAF renderer next) w
    loop

rAF :: (Context2D -> Effect Unit) -> (Unit -> Effect Unit) -> Effect Unit
rAF render next = do
  mCanvas <- getCanvasElementById "canvas"
  case mCanvas of
    Nothing -> error "No canvas"
    Just canvas -> do
      context <- getContext2D canvas
      clearRect context { x: 0.0, y: 0.0, width: canvasWidth, height: canvasHeight }
      render context
  next unit

runGameEngine :: forall w. Effect w -> GameSetup w -> StepFrameKeys w -> RenderFrame w -> Effect Unit
runGameEngine initWorld gameSetup gameFrame renderFrame = do
  world <- initWorld
  runContT (evalStateT (runGame gameSetup gameFrame renderFrame) world) pure
  pure unit
