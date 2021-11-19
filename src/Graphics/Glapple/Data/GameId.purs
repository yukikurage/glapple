-- | ゲームの状態を外部から操作する関数
module Graphics.Glapple.Data.GameId where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Graphics.Glapple.Data.Emitter (EmitterId, fire)
import Graphics.Glapple.Data.Picture (Picture(..))

data GameId input =
  GameId
    { inputEmitter :: EmitterId Effect input --Input EmitterはInputを取る必要がある
    , renderEmitter :: EmitterId Aff Unit --Render EmitterもgameStateを取る必要があるのでは？
    -- ない: gameStateは中で与えられる？から
    }

-- | GameIdで表されるゲームにInputを発火させます
tell
  :: forall input
   . GameId input
  -> input
  -> Effect Unit
tell (GameId { inputEmitter }) input = fire inputEmitter input

-- | GameIdで表されるゲームの状態を描画
renderGame
  :: forall sprite input
   . GameId input
  -> Picture sprite
renderGame (GameId { renderEmitter }) = Picture \_ _ -> fire renderEmitter unit
