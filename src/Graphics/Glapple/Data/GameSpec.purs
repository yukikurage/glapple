-- | 純粋な操作しか行わないゲーム
-- |
module Graphics.Glapple.Data.GameSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds)
import Graphics.Glapple.Data.Event (Event)
import Graphic.Glapple.GlappleM (GlappleM, getGameState, getTotalTime, putGameState)
import Graphics.Glapple.Data.GameSpecM (GameSpecM(..))
import Graphics.Glapple.Data.Picture (Picture, empty)

newtype GameSpec sprite gameState input = GameSpec
  { initGameState :: gameState
  , render :: Maybe Milliseconds -> gameState -> Picture sprite
  , eventHandler :: Event -> gameState -> gameState
  , inputHandler :: input -> gameState -> gameState
  }

mkGameSpecM
  :: forall gameState sprite input output
   . GameSpec sprite gameState input
  -> GameSpecM sprite gameState input output

mkGameSpecM
  ( GameSpec
      { initGameState
      , render
      , eventHandler
      , inputHandler
      }
  ) = GameSpecM
  { initGameState: mkInitGameStateM initGameState
  , render: mkRenderM render
  , eventHandler: mkHandlerM eventHandler
  , inputHandler: mkHandlerM inputHandler
  }

mkInitGameStateM :: forall f a. Applicative f => a -> f a
mkInitGameStateM = pure

mkRenderM
  :: forall sprite gameState output
   . (Maybe Milliseconds -> gameState -> Picture sprite)
  -> GlappleM sprite gameState output (Picture sprite)
mkRenderM f = do
  gameStateMaybe <- getGameState
  totalTime <- getTotalTime
  pure case gameStateMaybe of
    Just x -> f totalTime x
    Nothing -> empty

mkHandlerM
  :: forall a sprite gameState output
   . (a -> gameState -> gameState)
  -> a
  -> GlappleM sprite gameState output Unit
mkHandlerM f e = do
  gameStateMaybe <- getGameState
  putGameState case gameStateMaybe of
    Just x -> Just $ f e x
    Nothing -> Nothing