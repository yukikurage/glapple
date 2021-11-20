module TestComponents.DestroyTest where

import Prelude

import Effect.Class.Console (logShow)
import Graphics.Glapple (Event(..), GameSpecM(..), KeyCode(..), defaultHandler, destroyMe, getGameState, getKeyState, mkInitGameStateM, putGameState)
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
  , initGameState: mkInitGameStateM { x: 0.0 }
  }
  where
  eventHandler = case _ of
    Update { deltaTime } -> do
      logShow "Destroy Test Updated"
      { x } <- getGameState
      d <- getKeyState $ Keyboard "KeyD"
      when d $ putGameState $ { x: x + deltaTime * 80.0 }
      when (x > 160.0) destroyMe
    _ -> pure unit
  render = do
    { x } <- getGameState
    pure $ sprite Apple
      # translate (-16.0) (-16.0)
      # rotate (2.0 * pi * x / 200.0)
      # translate x 20.0