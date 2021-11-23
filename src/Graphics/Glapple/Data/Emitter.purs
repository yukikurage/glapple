module Graphics.Glapple.Data.Emitter (newEmitter, unregister, unregisterAll, register, fire, EmitterId, RegistrationId, emitterSize) where

import Prelude

import Data.Foldable (for_)
import Data.Map (Map, delete, empty, insert, size)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (randomInt)
import Effect.Ref (Ref, new, read, write)

type KeyId = Int /\ Int --KeyIdは2つの整数値であって，連番/\ランダム値

-- | イベントを発火させる中継点になる場所
-- | Emitterが発火 -> それに紐付けられたハンドラが発火
type Emitter m i = Int /\ Map KeyId (i -> m Unit) --連番 /\ ハンドラのMap

-- 基本的にこちらの形で使ってもらう
newtype EmitterId m i = EmitterId (Ref (Emitter m i))

-- | Emitterに登録した際に帰ってくるKey
data RegistrationId m i = RegistrationId (EmitterId m i) KeyId

-- | 新しいエミッタを作成
newEmitter :: forall m m' i. MonadEffect m => m (EmitterId m' i)
newEmitter = do
  emitterId <- liftEffect $ new $ bottom /\ empty
  pure $ EmitterId emitterId

-- | すべてのハンドラを削除
unregisterAll :: forall m m' i. MonadEffect m => EmitterId m' i -> m Unit
unregisterAll (EmitterId emitterId) = liftEffect $ write (bottom /\ empty) emitterId

-- | エミッタを発火させる
fire :: forall m i. MonadEffect m => Applicative m => EmitterId m i -> i -> m Unit
fire (EmitterId emitterRef) i = do
  _ /\ emitter <- liftEffect $ read emitterRef
  for_ emitter (\f -> f i)

-- | エミッタにハンドラを登録する
register :: forall m m' i. MonadEffect m => EmitterId m' i -> (i -> m' Unit) -> m (RegistrationId m' i)
register (EmitterId emitterRef) handler = do
  maxKey /\ emitter <- liftEffect $ read emitterRef
  r <- liftEffect $ randomInt bottom top
  let
    newKey = maxKey /\ r
    newMaxKey = if maxKey == top then bottom else maxKey + 1
    newE = newMaxKey /\ insert newKey handler emitter
  liftEffect $ write newE emitterRef
  pure (RegistrationId (EmitterId emitterRef) newKey)

-- | エミッタからハンドラを削除する
unregister :: forall m m' i. MonadEffect m => RegistrationId m' i -> m Unit
unregister (RegistrationId (EmitterId emitterRef) key) = do
  maxKey /\ emitter <- liftEffect $ read emitterRef
  let
    newE = maxKey /\ delete key emitter
  liftEffect $ write newE emitterRef

emitterSize :: forall m' i m. MonadEffect m => EmitterId m' i -> m Int
emitterSize (EmitterId emitterRef) = do
  _ /\ emitter <- liftEffect $ read emitterRef
  pure $ size emitter