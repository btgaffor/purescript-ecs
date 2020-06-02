module EcsCanvas where

import Prelude

import Control.Monad.State (evalStateT, execStateT)
import Data.Maybe (Maybe(..))
import Ecs (SystemT)
import Effect (Effect)
import Effect.Console (error, log)
import Effect.Ref (Ref, modify_, new, read)
import Graphics.Canvas (Context2D, clearRect, getCanvasElementById, getContext2D)
import Model (World)
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
  = SystemT w Effect Unit

type StepFrameKeys w
  = Keys -> SystemT w Effect Unit

type StepFrame w
  = SystemT w Effect Unit

type RenderFrame w
  = SystemT w Effect (Context2D -> Effect Unit)

startLoop :: Ref Keys -> World -> StepFrameKeys World -> RenderFrame World -> Effect Unit
startLoop keysRef initialState stepFrame renderFrame = do
  w <- HTML.window
  void $ HTML.Window.requestAnimationFrame (loop initialState) w
  where
    loop state = do
      keys <- read keysRef
      newState <- execStateT (stepFrame keys) state

      w <- HTML.window
      void $ HTML.Window.requestAnimationFrame (loop newState) w

      mCanvas <- getCanvasElementById "canvas"
      case mCanvas of
        Nothing -> error "No canvas"
        Just canvas -> do
          context <- getContext2D canvas
          clearRect context { x: 0.0, y: 0.0, width: canvasWidth, height: canvasHeight }
          renderer <- evalStateT renderFrame newState
          renderer context



runGameEngine :: Effect World -> GameSetup World -> StepFrameKeys World -> RenderFrame World -> Effect Unit
runGameEngine initWorld gameSetup gameFrame renderFrame = do
  world <- initWorld
  keysRef <- new initKeys
  setupInput keysRef
  initialState <- execStateT gameSetup world
  startLoop keysRef initialState gameFrame renderFrame
  pure unit
