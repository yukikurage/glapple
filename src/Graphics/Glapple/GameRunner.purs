module Graphics.Glapple.GameRunner where

import Prelude

import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Graphic.Glapple.GlappleM (GlappleM)
import Graphics.Canvas (CanvasElement)
import Graphics.Glapple.Data.GameId (GameId)
import Graphics.Glapple.Data.GameSpec (GameSpec, mkGameSpecM)
import Graphics.Glapple.Data.GameSpecM (CanvasSpec)
import Graphics.Glapple.GameRunnerM (runChildGameM_, runGameM_)

runChildGame
  :: forall s g i o childG childI childO
   . GameSpec s childG childI
  -> GlappleM s g i o (GameId s childI childO)
runChildGame gameSpec = runChildGameM_ (mkGameSpecM gameSpec)

runGame
  :: forall sprite gameState input output
   . Ord sprite
  => Int
  -> CanvasElement
  -> CanvasSpec
  -> Array (sprite /\ String)
  -> GameSpec sprite gameState input
  -> Effect (GameId sprite input output)
runGame fps canvasElement canvasSpec sprites gameSpec =
  runGameM_ fps canvasElement canvasSpec sprites (mkGameSpecM gameSpec)