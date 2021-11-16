module Graphics.Glapple.Data.CanvasSpec where

import Color (Color)
import Graphics.Canvas (CanvasElement)

data CanvasSpec = CanvasSpec
  { canvasElement :: CanvasElement
  , height :: Number
  , width :: Number
  , bgColor :: Color
  }