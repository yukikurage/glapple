-- | ゲームの状態を外部から操作する関数
module Graphics.Glapple.Data.GameId where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Graphics.Canvas (CanvasImageSource, Context2D)
import Graphics.Glapple.Data.Emitter (EmitterId, fire, newEmitter)
import Graphics.Glapple.Data.Picture (Picture, absorb', empty)

-- | ゲームを指し示すIDです．
-- | これを使って外部からゲームに指示できます．
data GameId s i =
  GameId
    { inputEmitter :: EmitterId Effect i --Input EmitterはInputを取る必要がある
    , renderEmitter ::
        EmitterId Aff
          { canvasImageSources :: s -> Maybe CanvasImageSource
          , context2D :: Context2D
          }
    }

-- | GameIdで表されるゲームにInputを発火させます
tell
  :: forall s i m
   . MonadEffect m
  => GameId s i
  -> i
  -> m Unit
tell (GameId { inputEmitter }) input = liftEffect $ fire inputEmitter $ input

-- | GameIdで表されるゲームの状態を描画
renderGame
  :: forall s i
   . GameId s i
  -> Picture s
renderGame (GameId { renderEmitter }) = absorb' \context2D canvasImageSources -> do
  fire renderEmitter { context2D, canvasImageSources }
  pure empty

emptyGameId :: forall m s i. Bind m => MonadEffect m => m (GameId s i)
emptyGameId = do
  inputEmitter <- newEmitter
  renderEmitter <- newEmitter
  pure $ GameId { inputEmitter, renderEmitter }