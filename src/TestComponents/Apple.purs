module TestComponents.Apple where

import Prelude

import Effect.Class.Console (logShow)
import Graphics.Glapple (Event(..), GameSpecM(..), KeyCode(..), KeyState(..), getGameState, getTotalTime, mkHandlerM, mkInitGameStateM, modifyGameState)
import Graphics.Glapple.Data.Picture (rotate, scale, sprite, translate)
import Math (pi)
import TestComponents.Sprites (Sprite(..))

data Input = StartRotate
type GameState = { rotating :: Boolean }

gameSpec
  :: forall o
   . GameSpecM Sprite GameState Input o
gameSpec = GameSpecM
  { eventHandler
  , inputHandler
  , render
  , initGameState: mkInitGameStateM { rotating: false }
  }
  where
  eventHandler = case _ of
    KeyEvent { keyCode: Keyboard "KeyW", keyState: KeyDown } -> modifyGameState \{ rotating } -> { rotating: not rotating }
    KeyEvent { keyCode: Keyboard s } -> logShow s
    _ -> pure unit
  inputHandler = mkHandlerM case _ of
    StartRotate -> \_ -> { rotating: true }
  render = do
    time <- getTotalTime
    { rotating } <- getGameState
    let
      revolution = if rotating then rotate (2.0 * pi * time * 2.0) else identity
    pure $ sprite Apple
      # translate (-16.0) (-16.0)
      # scale 6.0 6.0
      # rotate (2.0 * pi * time * 5.0)
      # translate 80.0 80.0
      # revolution
      # translate 160.0 160.0