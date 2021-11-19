module Graphic.Glapple.GlappleM where

import Prelude

import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Control.Monad.State (lift)
import Data.Maybe (Maybe(..))
import Data.Time (Time, diff)
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (throw)
import Effect.Now (nowTime)
import Effect.Ref (Ref, modify_, read, write)
import Graphics.Glapple.Data.Emitter (EmitterId, fire)
import Graphics.Glapple.Data.Event (Event)
import Graphics.Glapple.Data.InternalRegistrationIds (InternalRegistrationIds, unregisterGame)

type InternalState (s :: Type) g (i :: Type) o =
  { eventEmitter :: EmitterId Effect Event
  , outputEmitter :: EmitterId Effect o
  , initTimeRef :: Ref (Maybe Time) --ゲーム開始時の時刻
  , gameStateRef :: Ref (Maybe g) --ゲームの状態を保存(ゲーム開始前はNothing)
  , internalRegistrationIdsRef :: Ref (Maybe (InternalRegistrationIds s i o)) --ゲームのregistrationIdを保存
  }

newtype GlappleM s g i o a =
  GlappleM (ReaderT (InternalState s g i o) Effect a)

derive newtype instance Functor (GlappleM s g i o)
derive newtype instance Apply (GlappleM s g i o)
derive newtype instance Applicative (GlappleM s g i o)
derive newtype instance Bind (GlappleM s g i o)
derive newtype instance Monad (GlappleM s g i o)
derive newtype instance
  MonadAsk (InternalState s g i o)
    (GlappleM s g i o)

runGlappleM
  :: forall s g i o a
   . GlappleM s g i o a
  -> InternalState s g i o
  -> Effect a
runGlappleM (GlappleM state) f = runReaderT state f

instance MonadEffect (GlappleM s g i o) where
  liftEffect e = GlappleM $ lift e

getGameState
  :: forall s g i o
   . GlappleM s g i o g
getGameState = do
  { gameStateRef } <- ask
  gameState <- liftEffect $ read gameStateRef
  case gameState of
    Just x -> pure x
    Nothing -> liftEffect $ throw "Glapple Warning: ゲームが始まる前にgameStateを取得しようとしました"

putGameState :: forall s g i o. g -> GlappleM s g i o Unit
putGameState x = do
  { gameStateRef } <- ask
  liftEffect $ write (Just x) gameStateRef

modifyGameState
  :: forall s g i o
   . (g -> g)
  -> GlappleM s g i o Unit
modifyGameState f = do
  { gameStateRef } <- ask
  liftEffect $ modify_ (map f) gameStateRef

getTotalTime
  :: forall s g i o
   . GlappleM s g i o Milliseconds
getTotalTime = do
  { initTimeRef } <- ask
  initTimeMaybe <- liftEffect $ read initTimeRef
  nowT <- liftEffect $ nowTime
  case initTimeMaybe of
    Just initTime -> pure $ (diff nowT initTime)
    Nothing -> liftEffect $ throw "Glapple Warning: ゲームが始まる前にtotalTimeを取得しようとしました"

raise
  :: forall s g i o
   . o
  -> GlappleM s g i o Unit
raise output = do
  { outputEmitter } <- ask --outputのエミッターを取得
  liftEffect $ fire outputEmitter output --発火

destroyMe :: forall s g i o. GlappleM s g i o Unit
destroyMe = do
  { internalRegistrationIdsRef } <- ask
  internalRegistrationIdsMaybe <- liftEffect $ read internalRegistrationIdsRef
  case internalRegistrationIdsMaybe of
    Just x -> unregisterGame x
    Nothing -> liftEffect $ throw "初期化前にゲームを破棄することはできません"