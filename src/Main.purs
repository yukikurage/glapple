module Main where

import Prelude

import Color (Color, rgba')
import Data.Array (fold)
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Graphic.Glapple.Data.Event (Event(..))
import Graphic.Glapple.GlappleM (GlappleM(..), getTotalTime)
import Graphics.Canvas (CanvasElement, TextAlign(..), TextBaseline(..), getCanvasElementById)
import Graphics.Glapple.Data.CanvasSpec (CanvasSpec(..))
import Graphics.Glapple.Data.Complex (i, (*~))
import Graphics.Glapple.Data.GameSpecEffect (GameSpecEffect(..))
import Graphics.Glapple.Data.GameSpecEffect (runGame)
import Graphics.Glapple.Data.Picture (DrawStyle(..), FillStyle(..), Font(..), FontFamily(..), FontStyle(..), FontWeight(..), Picture(..), rotate, sprite, text, translate)
import Math (pi)

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

derive instance Eq Sprite
derive instance Ord Sprite

type GameState = { fps :: Number }

type Input = Unit
type Output = Unit

sprites :: Array (Sprite /\ String)
sprites = [ Apple /\ "/images/apple.png" ]

handler e x = case e of
  Update (Milliseconds t) -> pure { fps: 1000.0 / t }
  _ -> pure x

initGameState = { fps: 0.0 }

render :: forall o. GameState -> GlappleM o (Picture Sprite)
render { fps } = do
  timeMaybe <- getTotalTime
  let
    fpsText = translate 320.0 5.0 $ text
      (Fill (FillColor $ rgba' 1.0 0.0 0.0 1.0))
      AlignRight
      BaselineHanging
      fontStandard
      (show $ floor fps)
    time = case timeMaybe of
      Just (Milliseconds x) -> x
      Nothing -> 0.0
    rotateApple = sprite Apple
      # translate (-16.0) (-16.0)
      # rotate (2.0 * pi * time / 200.0)
      # translate 80.0 80.0
      # rotate (2.0 * pi * time / 1000.0)
      # translate 160.0 160.0
  pure $ fpsText <> rotateApple

fontStandard = Font
  { fontFamily: Monospace
  , fontHeight: 40
  , fontSize: 20
  , fontWeight: Bold
  , fontStyle: FontStyleNormal
  }

gameSpec :: CanvasElement -> GameSpecEffect Sprite GameState Input Output
gameSpec canvas = GameSpecEffect
  { canvasSpec: CanvasSpec
      { canvasElement: canvas
      , width: 320.0
      , height: 320.0
      }
  , fps: 60
  , handler
  , initGameState
  , render
  , sprites
  }