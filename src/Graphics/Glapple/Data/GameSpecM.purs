module Graphics.Glapple.Data.GameSpecM where

import Data.Tuple.Nested (type (/\))
import Graphic.Glapple.Data.Event (Event)
import Graphics.Glapple.Data.CanvasSpec (CanvasSpec)
import Graphics.Glapple.Data.Picture (Picture)

newtype GameSpecM spriteId gameState input output m = GameSpecM
  { fps :: Int
  , canvasSpec :: CanvasSpec
  , preLoad :: Array (spriteId /\ String)
  , initGameState :: gameState
  , render :: gameState -> m (Picture spriteId)
  , handler :: Event input -> gameState -> m gameState
  }