module TestComponents.DestroyTest where

import Prelude

import Graphics.Glapple (Event(..), GameSpecM(..), KeyCode(..), KeyState(..), defaultHandler, destroy, getGameState, getKeyState, putGameState)
import Graphics.Glapple.Data.Picture (rotate, sprite, translate)
import Math (pi)
import TestComponents.Sprites (Sprite(..))

type GameState = { x :: Number }

gameSpec
  :: forall i o
   . GameSpecM Sprite GameState i o
gameSpec = GameSpecM
  { eventHandler
  , inputHandler: defaultHandler
  , render
  , initGameState: pure { x: 0.0 }
  }
  where
  eventHandler = case _ of
    Update { deltaTime } -> do
      { x } <- getGameState
      d <- getKeyState $ Keyboard "KeyD"
      when d $ putGameState $ { x: x + deltaTime * 80.0 }
      when (x > 160.0) destroy
    KeyEvent { keyCode: Keyboard "KeyS", keyState: KeyDown } -> destroy
    _ -> pure unit
  render = do
    { x } <- getGameState
    pure $ sprite Apple
      # translate (-16.0) (-16.0)
      # rotate (2.0 * pi * x / 200.0)
      # translate x 20.0