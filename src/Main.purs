module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay, runAff_)
import Effect.Class (liftEffect)
import Graphics.Canvas (getCanvasElementById)
import Graphics.Glapple (runGameM_, tell)
import TestComponents.Root (gameSpec)
import TestComponents.Sprites (sprites)

main :: Effect Unit
main = do
  canvasMaybe <- getCanvasElementById "canvas"
  case canvasMaybe of
    Nothing -> pure unit
    Just canvas -> do
      gameId <- runGameM_ 60 canvas { width: 320.0, height: 320.0 } sprites gameSpec
      runAff_ (const $ pure unit) do
        delay $ Milliseconds 3000.0
        liftEffect $ tell gameId unit
