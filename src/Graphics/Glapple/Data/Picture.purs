module Graphics.Glapple.Data.Picture where

import Prelude

import Effect (Effect)
import Graphics.Canvas (CanvasImageSource)

data Picture spriteId = Picture ((spriteId -> CanvasImageSource) -> Effect Unit)