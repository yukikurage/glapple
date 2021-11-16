module Graphics.Glapple where

import Graphic.Glapple.Data.Event (Event)
import Graphics.Glapple.Data.CanvasSpec (CanvasSpec)
import Graphics.Glapple.Data.Picture (Picture)

newtype GameSpec gameState sprite = GameSpec
  { fps :: Int
  , canvasSpec :: CanvasSpec
  , initGameState :: gameState
  , render :: (sprite -> Picture) -> gameState -> Picture
  , update :: Number -> gameState -> gameState
  , eventHandler :: Event -> gameState -> gameState
  }