module Graphics.Glapple.GameRunner where

import Prelude

import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Graphics.Canvas (CanvasElement)
import Graphics.Glapple.Data.GameId (GameId)
import Graphics.Glapple.Data.GameSpec (GameSpec, mkGameSpecM)
import Graphics.Glapple.Data.GameSpecM (CanvasSpec)
import Graphics.Glapple.GameRunnerM (runGameM_)

runGame
  :: forall sprite gameState input
   . Ord sprite
  => Int
  -> CanvasElement
  -> CanvasSpec
  -> Array (sprite /\ String)
  -> GameSpec sprite gameState input
  -> Effect (GameId input)
runGame fps canvasElement canvasSpec sprites gameSpec =
  runGameM_ fps canvasElement canvasSpec sprites (mkGameSpecM gameSpec)