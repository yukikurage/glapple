module Graphics.Glapple.Data.CanvasSpec where

import Graphics.Canvas (CanvasElement)

newtype CanvasSpec = CanvasSpec
  { canvasElement :: CanvasElement
  , height :: Number
  , width :: Number
  }