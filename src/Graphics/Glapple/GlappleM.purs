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

-- | 非推奨です(使わないほうがよい)
type InternalState gameState output =
  { outputHandler :: output -> Effect Unit
  , initTimeRef :: Ref (Maybe Time)
  , gameStateRef :: Ref gameState
  }

newtype GlappleM gameState output a =
  GlappleM (ReaderT (InternalState gameState output) Effect a)

derive newtype instance Functor (GlappleM gameState output)
derive newtype instance Apply (GlappleM gameState output)
derive newtype instance Applicative (GlappleM gameState output)
derive newtype instance Bind (GlappleM gameState output)
derive newtype instance Monad (GlappleM gameState output)
derive newtype instance MonadAsk (InternalState gameState output) (GlappleM gameState output)

runGlappleM
  :: forall gameState output a
   . GlappleM gameState output a
  -> InternalState gameState output
  -> Effect a
runGlappleM (GlappleM state) f = runReaderT state f

instance MonadEffect (GlappleM gameState output) where
  liftEffect e = GlappleM $ lift e

getGameState :: forall gameState output. GlappleM gameState output gameState
getGameState = do
  { gameStateRef } <- ask
  liftEffect $ read gameStateRef

putGameState :: forall gameState output. gameState -> GlappleM gameState output Unit
putGameState x = do
  { gameStateRef } <- ask
  liftEffect $ write x gameStateRef

modifyGameState
  :: forall gameState output
   . (gameState -> gameState)
  -> GlappleM gameState output Unit
modifyGameState f = do
  { gameStateRef } <- ask
  liftEffect $ modify_ f gameStateRef

getTotalTime :: forall gameState output. GlappleM gameState output (Maybe Milliseconds)
getTotalTime = do
  { initTimeRef } <- ask
  initTimeMaybe <- liftEffect $ read initTimeRef
  nowT <- liftEffect $ nowTime
  case initTimeMaybe of
    Just initTime -> pure $ Just $ diff nowT initTime
    Nothing -> pure Nothing

raise :: forall gameState output. output -> GlappleM gameState output Unit
raise output = do
  { outputHandler } <- ask
  liftEffect $ outputHandler output