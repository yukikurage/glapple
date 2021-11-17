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
import Effect.Ref (Ref, read)

type InternalState output = { outputHandler :: output -> Effect Unit, initTimeRef :: Ref (Maybe Time) }

newtype GlappleM output a = GlappleM (ReaderT (InternalState output) Effect a)

derive newtype instance Functor (GlappleM output)
derive newtype instance Apply (GlappleM output)
derive newtype instance Applicative (GlappleM output)
derive newtype instance Bind (GlappleM output)
derive newtype instance Monad (GlappleM output)
derive newtype instance MonadAsk (InternalState output) (GlappleM output)

runGlappleM :: forall output a. GlappleM output a -> InternalState output -> Effect a
runGlappleM (GlappleM state) f = runReaderT state f

instance MonadEffect (GlappleM output) where
  liftEffect e = GlappleM $ lift e

getTotalTime :: forall output. GlappleM output (Maybe Milliseconds)
getTotalTime = do
  { initTimeRef } <- ask
  initTimeMaybe <- liftEffect $ read initTimeRef
  nowT <- liftEffect $ nowTime
  case initTimeMaybe of
    Just initTime -> pure $ Just $ diff nowT initTime
    Nothing -> pure Nothing

raise :: forall output. output -> GlappleM output Unit
raise output = do
  { outputHandler } <- ask
  liftEffect $ outputHandler output