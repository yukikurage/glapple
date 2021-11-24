module Main where

import Prelude

import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, runAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import GlappleExamples.Games.Root as Root
import GlappleExamples.Sprites (sprites)
import Graphics.Canvas (getCanvasElementById)
import Graphics.Glapple (runGameM_, tell)

main :: Effect Unit
main = do
  canvas <- maybe (throw "Can not find canvas element") pure
    =<< getCanvasElementById "canvas"
  gameId <- runGameM_ 60.0 canvas { width: 320.0, height: 320.0 } sprites Root.gameSpec
  runAff_ (const $ pure unit) do
    delay $ Milliseconds 3000.0
    liftEffect $ tell gameId $ unit
