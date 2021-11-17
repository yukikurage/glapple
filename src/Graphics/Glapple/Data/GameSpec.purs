-- | 純粋な操作しか行わないゲーム
-- |
module Graphics.Glapple.Data.GameSpec where

import Prelude

import Data.Maybe (Maybe)
import Data.Time.Duration (Milliseconds)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Graphic.Glapple.Data.Event (Event)
import Graphic.Glapple.GlappleM (GlappleM, getGameState, getTotalTime, putGameState)
import Graphics.Canvas (CanvasElement)
import Graphics.Glapple.Data.GameId (GameId)
import Graphics.Glapple.Data.GameSpecM (CanvasSpec, GameSpecM(..), runGameM)
import Graphics.Glapple.Data.Picture (Picture)

newtype GameSpec sprite gameState input = GameSpec
  { fps :: Int
  , canvasSpec :: CanvasSpec
  , sprites :: Array (sprite /\ String)
  , initGameState :: gameState
  , render :: Maybe Milliseconds -> gameState -> Picture sprite
  , handler :: Event input -> gameState -> gameState
  }

runGame
  :: forall sprite gameState input output
   . Ord sprite
  => GameSpec sprite gameState input
  -> CanvasElement
  -> Effect (GameId gameState input output)
runGame gameSpec canvasElement = runGameM (mkGameSpecM gameSpec) canvasElement (\_ -> pure unit)

mkGameSpecM
  :: forall gameState sprite input output
   . GameSpec sprite gameState input
  -> GameSpecM sprite gameState input output
mkGameSpecM
  ( GameSpec
      { fps
      , canvasSpec
      , initGameState
      , sprites
      , render
      , handler
      }
  ) = GameSpecM
  { fps
  , canvasSpec
  , initGameState
  , sprites
  , render: mkRenderM render
  , handler: mkHandlerM handler
  }

mkRenderM
  :: forall gameState output sprite
   . (Maybe Milliseconds -> gameState -> Picture sprite)
  -> GlappleM gameState output (Picture sprite)
mkRenderM f = f <$> getTotalTime <*> getGameState

mkHandlerM
  :: forall gameState input output
   . (Event input -> gameState -> gameState)
  -> Event input
  -> GlappleM gameState output Unit

mkHandlerM f e = putGameState =<< f e <$> getGameState