module Graphics.Glapple.Data.Emitter where

import Prelude

import Data.Foldable (for_)
import Data.Map (Map, delete, empty, findMax, insert)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref, new, read, write)

-- | イベントを発火させる中継点になる場所
-- | Emitterが発火 -> それに紐付けられたハンドラが発火
type Emitter m i = Map Int (i -> m Unit)

-- 基本的にこちらの形で使ってもらう
newtype EmitterId m i = EmitterId (Ref (Emitter m i))

-- | Emitterに登録した際に帰ってくるKey
data RegistrationId m i = RegistrationId (EmitterId m i) Int

-- | 新しいエミッタを作成
newEmitter :: forall m m' i. MonadEffect m => m (EmitterId m' i)
newEmitter = do
  emitterId <- liftEffect $ new empty
  pure $ EmitterId emitterId

-- | すべてのハンドラを削除
unregisterAll :: forall m m' i. MonadEffect m => EmitterId m' i -> m Unit
unregisterAll (EmitterId emitterId) = liftEffect $ write empty emitterId

-- | エミッタを発火させる
fire :: forall m i. MonadEffect m => Applicative m => EmitterId m i -> i -> m Unit
fire (EmitterId emitterRef) i = do
  emitter <- liftEffect $ read emitterRef
  for_ emitter (\f -> f i)

-- | エミッタにハンドラを登録する
register :: forall m m' i. MonadEffect m => EmitterId m' i -> (i -> m' Unit) -> m (RegistrationId m' i)
register (EmitterId emitterRef) handler = do
  emitter <- liftEffect $ read emitterRef
  let
    newKey = case findMax emitter of
      Just { key } -> key + 1
      Nothing -> 0
    newE = insert newKey handler emitter
  liftEffect $ write newE emitterRef
  pure (RegistrationId (EmitterId emitterRef) newKey)

-- | エミッタからハンドラを削除する
unregister :: forall m m' i. MonadEffect m => RegistrationId m' i -> m Unit
unregister (RegistrationId (EmitterId emitterRef) key) = do
  emitter <- liftEffect $ read emitterRef
  let
    newE = delete key emitter
  liftEffect $ write newE emitterRef