-- | ゲームの状態を外部から操作する関数
module Graphics.Glapple.Data.GameId where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Graphics.Canvas (CanvasImageSource, Context2D)
import Graphics.Glapple.Data.Emitter (EmitterId, fire)
import Graphics.Glapple.Data.InternalRegistrationIds (InternalRegistrationIds, unregisterGame)
import Graphics.Glapple.Data.Picture (Picture(..))

-- | ゲームを指し示すIDです．
-- | これを使って外部からゲームに指示できます．
data GameId s i o =
  GameId
    { inputEmitter :: EmitterId Effect i --Input EmitterはInputを取る必要がある
    , renderEmitter ::
        EmitterId Aff
          { canvasImageSources :: s -> Maybe CanvasImageSource
          , context2D :: Context2D
          } --Render EmitterもgameStateを取る必要があるのでは？
    , internalRegistrationIds :: InternalRegistrationIds s i o
    }

-- | GameIdで表されるゲームにInputを発火させます
tell
  :: forall s i o m
   . MonadEffect m
  => GameId s i o
  -> i
  -> m Unit
tell (GameId { inputEmitter }) input = liftEffect $ fire inputEmitter input

-- | GameIdで表されるゲームの状態を描画
renderGame
  :: forall s i o
   . GameId s i o
  -> Picture s
renderGame (GameId { renderEmitter }) = Picture \context2D canvasImageSources ->
  fire renderEmitter { context2D, canvasImageSources }

destroy :: forall m s i o. MonadEffect m => GameId s i o -> m Unit
destroy (GameId { internalRegistrationIds }) = unregisterGame internalRegistrationIds