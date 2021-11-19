-- | runGameMでゲームを実行します．
module Graphics.Glapple.Data.GameSpecM
  ( GameSpecM(..)
  , CanvasSpec
  , defaultRender
  , defaultHandler
  ) where

import Prelude

import Graphic.Glapple.Data.Event (Event)
import Graphic.Glapple.GlappleM (GlappleM)
import Graphics.Glapple.Data.Picture (Picture, empty)

type CanvasSpec =
  { height :: Number
  , width :: Number
  }

newtype GameSpecM s g i o = GameSpecM
  { initGameState :: GlappleM s g o g
  , render :: GlappleM s g o (Picture s)
  , inputHandler :: i -> GlappleM s g o Unit
  , eventHandler :: Event -> GlappleM s g o Unit
  }

defaultRender :: forall m s. Applicative m => m (Picture s)
defaultRender = pure empty

defaultHandler :: forall m a. Applicative m => a -> m Unit
defaultHandler = const $ pure unit
