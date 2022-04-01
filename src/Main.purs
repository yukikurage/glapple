module Main where

import Prelude

import Components.ColliderTest (colliderTest)
import Components.Root (root)
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Exception (throw)
import Graphics.Canvas (getCanvasElementById)
import Graphics.Glapple.Data.Complex (complex)
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
    , initMousePosition: complex 0.0 0.0
    }
    root
    {}
