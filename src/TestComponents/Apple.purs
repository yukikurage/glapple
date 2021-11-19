module TestComponents.Apple where

import Prelude

import Graphics.Glapple (Event(..), GameSpecM(..), KeyState(..), getGameState, getTotalTime, mkHandlerM, mkInitGameStateM)
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
  eventHandler = mkHandlerM case _ of
    KeyEvent "w" KeyDown -> \{ rotating } -> { rotating: not rotating }
    _ -> identity
  inputHandler = mkHandlerM case _ of
    StartRotate -> \_ -> { rotating: true }
  render = do
    time <- getTotalTime
    { rotating } <- getGameState
    let
      revolution = if rotating then rotate (2.0 * pi * time * 2.0) else identity
    pure $ sprite Apple
      # translate (-16.0) (-16.0)
      # scale 5.0 5.0
      # rotate (2.0 * pi * time * 5.0)
      # translate 80.0 80.0
      # revolution