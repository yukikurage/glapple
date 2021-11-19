module Graphic.Glapple.GlappleM where

import Prelude

import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Control.Monad.State (lift)
import Data.Maybe (Maybe(..))
import Data.Time (Time, diff)
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now (nowTime)
import Effect.Ref (Ref, modify_, read, write)
import Graphics.Glapple.Data.Event (Event)
import Graphics.Canvas (CanvasImageSource, Context2D)
import Graphics.Glapple.Data.Emitter (EmitterId, fire)

type InternalState sprite gameState output =
  { eventEmitter :: EmitterId Effect Event
  , outputEmitter :: EmitterId Effect output
  , initTimeRef :: Ref (Maybe Time) --ゲーム開始時の時刻
  , gameStateRef :: Ref (Maybe (gameState)) --ゲームの状態を保存(ゲーム開始前はNothing)
  , canvasImageSources :: (sprite -> Maybe CanvasImageSource) --読み込んだ画像一覧を保存
  , context2D :: Context2D
  }

newtype GlappleM sprite gameState output a =
  GlappleM (ReaderT (InternalState sprite gameState output) Effect a)

derive newtype instance Functor (GlappleM sprite gameState output)
derive newtype instance Apply (GlappleM sprite gameState output)
derive newtype instance Applicative (GlappleM sprite gameState output)
derive newtype instance Bind (GlappleM sprite gameState output)
derive newtype instance Monad (GlappleM sprite gameState output)
derive newtype instance
  MonadAsk (InternalState sprite gameState output)
    (GlappleM sprite gameState output)

runGlappleM
  :: forall sprite gameState output a
   . GlappleM sprite gameState output a
  -> InternalState sprite gameState output
  -> Effect a
runGlappleM (GlappleM state) f = runReaderT state f

instance MonadEffect (GlappleM sprite gameState output) where
  liftEffect e = GlappleM $ lift e

getGameState
  :: forall sprite gameState output
   . GlappleM sprite gameState output (Maybe gameState)
getGameState = do
  { gameStateRef } <- ask
  liftEffect $ read gameStateRef

putGameState :: forall sprite gameState output. Maybe gameState -> GlappleM sprite gameState output Unit
putGameState x = do
  { gameStateRef } <- ask
  liftEffect $ write x gameStateRef

modifyGameState
  :: forall sprite gameState output
   . (gameState -> gameState)
  -> GlappleM sprite gameState output Unit
modifyGameState f = do
  { gameStateRef } <- ask
  liftEffect $ modify_ (map f) gameStateRef

getTotalTime
  :: forall sprite gameState output
   . GlappleM sprite gameState output (Maybe Milliseconds)
getTotalTime = do
  { initTimeRef } <- ask
  initTimeMaybe <- liftEffect $ read initTimeRef
  nowT <- liftEffect $ nowTime
  case initTimeMaybe of
    Just initTime -> pure $ Just $ diff nowT initTime
    Nothing -> pure Nothing

raise
  :: forall sprite gameState output
   . output
  -> GlappleM sprite gameState output Unit
raise output = do
  { outputEmitter } <- ask --outputのエミッターを取得
  liftEffect $ fire outputEmitter output --発火