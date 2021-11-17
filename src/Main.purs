module Main where

import Prelude

import Color (Color, rgba')
import Data.Array (fold)
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Graphic.Glapple.Data.Event (Event(..))
import Graphics.Canvas (TextAlign(..), TextBaseline(..), getCanvasElementById)
import Graphics.Glapple.Data.CanvasSpec (CanvasSpec(..))
import Graphics.Glapple.Data.GameSpecEffect (GameSpecEffect(..))
import Graphics.Glapple.Data.GameSpecEffect (runGame)
import Graphics.Glapple.Data.Picture (DrawStyle(..), FillStyle(..), Font(..), FontFamily(..), FontStyle(..), FontWeight(..), fillText, sprite, translate)

main :: Effect Unit
main = do
  canvasMaybe <- getCanvasElementById "canvas"
  case canvasMaybe of
    Nothing -> pure unit
    Just canvas -> do
      _ <- runGame (gameSpec canvas) \_ -> pure unit
      pure unit

-- テスト用のゲーム

data Sprite = Apple

type GameState = { fps :: Number }

derive instance Eq Sprite
derive instance Ord Sprite

sprites =
  [ Apple /\ "/images/apple.png"
  ]

handler e x = case e of
  Update (Milliseconds t) -> pure { fps: 1000.0 / t }
  _ -> pure x

initGameState = { fps: 0.0 }

render { fps } = do
  pure $ translate { translateX: 320.0, translateY: 0.0 } $ fillText
    (Fill (FillColor $ rgba' 1.0 0.0 0.0 1.0))
    AlignRight
    BaselineHanging
    fontStandard
    (show $ floor fps)

fontStandard = Font
  { fontFamily: Monospace
  , fontHeight: 40
  , fontSize: 20
  , fontWeight: Bold
  , fontStyle: FontStyleNormal
  }

gameSpec canvas = GameSpecEffect
  { canvasSpec: CanvasSpec
      { canvasElement: canvas
      , width: 320.0
      , height: 320.0
      }
  , fps: 30
  , handler
  , initGameState
  , render
  , sprites
  }