module Graphics.Glapple.Data.GameSlot where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Graphics.Canvas (CanvasImageSource, Context2D)
import Graphics.Glapple.Data.Emitter (EmitterId, fire, newEmitter)
import Graphics.Glapple.Data.Picture (Picture(..))

-- | GameSlotは複数のゲームをまとめたもの
-- | GameIdと同じようにtellSlotやrenderGameSlotができるが，destroyはできない
data GameSlot s i =
  GameSlot
    { inputEmitter :: EmitterId Effect i
    , renderEmitter ::
        EmitterId Aff
          { canvasImageSources :: s -> Maybe CanvasImageSource
          , context2D :: Context2D
          }
    }

-- | GameSlotで表されるゲームにInputを発火
tellSlot
  :: forall s i
   . GameSlot s i
  -> i
  -> Effect Unit
tellSlot (GameSlot { inputEmitter }) input = fire inputEmitter input

-- | GameIdで表されるゲームの状態を描画
renderGameSlot
  :: forall s i
   . GameSlot s i
  -> Picture s
renderGameSlot (GameSlot { renderEmitter }) = Picture \context2D canvasImageSources ->
  fire renderEmitter { context2D, canvasImageSources }

-- | 空のゲームスロット
emptyGameSlot :: forall m s i. MonadEffect m => m (GameSlot s i)
emptyGameSlot = do
  inputEmitter <- newEmitter
  renderEmitter <- newEmitter
  pure $ GameSlot { inputEmitter, renderEmitter }