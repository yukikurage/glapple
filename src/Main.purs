module Main where

import Prelude

import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (throw)
import GlappleExamples.Games.Root (root)
import GlappleExamples.Sprites (sprites)
import Graphics.Canvas (getCanvasElementById)
import Graphics.Glapple.Runner (run)

main :: Effect Unit
main = do
  canvas <- maybe (throw "Can not find canvas element") pure
    =<< getCanvasElementById "canvas"
  launchAff_ $ run 60.0 sprites canvas root unit
