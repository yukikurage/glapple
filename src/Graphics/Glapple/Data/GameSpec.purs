module Graphics.Glapple.Data.GameSpec where

import Prelude

import Graphics.Glapple.GlappleM (GlappleM, getGameState, getTotalTime, putGameState)
import Graphics.Glapple.Data.Event (Event)
import Graphics.Glapple.Data.GameSpecM (GameSpecM(..))
import Graphics.Glapple.Data.Picture (Picture)

-- | A pure version of GameSpecM.
newtype GameSpec sprite gameState input = GameSpec
  { initGameState :: gameState
  , render :: { totalTime :: Number } -> gameState -> Picture sprite
  , eventHandler :: Event -> gameState -> gameState
  , inputHandler :: input -> gameState -> gameState
  }

-- | Convert GameSpec to GameSpecM.
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
  :: forall sprite gameState i output
   . ({ totalTime :: Number } -> gameState -> Picture sprite)
  -> GlappleM sprite gameState i output (Picture sprite)
mkRenderM f = do
  gameState <- getGameState
  totalTime <- getTotalTime
  pure $ f { totalTime } gameState

mkHandlerM
  :: forall a sprite gameState i output
   . (a -> gameState -> gameState)
  -> a
  -> GlappleM sprite gameState i output Unit
mkHandlerM f e = do
  gameState <- getGameState
  putGameState $ f e gameState