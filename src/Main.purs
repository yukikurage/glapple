module Main where

import Prelude

import Components.Root (root)
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Exception (throw)
import Graphics.Canvas (getCanvasElementById)
import Graphics.Glapple.Data.Component (Component)
import Graphics.Glapple.Runner (runGame)
import Sprites (sprites)

main :: Effect Unit
main = do
  canvas <- maybe (throw "Can not find canvas element") pure
    =<< getCanvasElementById "canvas"
  runGame
    { canvas
    , height: 500.0
    , width: 500.0
    , fps: 60.0
    , sprites
    , initMousePosition: { x: 0.0, y: 0.0 }
    }
    root {}
