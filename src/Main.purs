module Main where

import Prelude

import Color (rgba')
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (delay, runAff_)
import Effect.Class (liftEffect)
import Graphic.Glapple.Data.Event (Event(..))
import Graphic.Glapple.GlappleM (GlappleM, getGameState, getTotalTime, modifyGameState)
import Graphics.Canvas (TextAlign(..), TextBaseline(..), getCanvasElementById)
import Graphics.Glapple.Data.GameId (tell)
import Graphics.Glapple.Data.GameSpecM (GameSpecM(..), runGame)
import Graphics.Glapple.Data.Picture (DrawStyle(..), FillStyle(..), Font(..), FontFamily(..), FontStyle(..), FontWeight(..), Picture, rotate, scale, sprite, text, translate)
import Math (pi)

main :: Effect Unit
main = do
  canvasMaybe <- getCanvasElementById "canvas"
  case canvasMaybe of
    Nothing -> pure unit
    Just canvas -> do
      gameId <- runGame gameSpec canvas \_ -> pure unit
      runAff_ (const $ pure unit) do
        delay $ Milliseconds 10000.0
        liftEffect $ tell gameId unit

-- テスト用のゲーム

data Sprite = Apple

derive instance Eq Sprite
derive instance Ord Sprite

type GameState = { fps :: Number, rotating :: Boolean }

type Input = Unit
type Output = Unit

sprites :: Array (Sprite /\ String)
sprites = [ Apple /\ "/images/apple.png" ]

handler :: forall i o. Event i -> GlappleM GameState o Unit
handler = case _ of
  Update (Milliseconds t) -> modifyGameState $ _ { fps = 1000.0 / t }
  Input _ -> modifyGameState $ _ { rotating = true }
  _ -> pure unit

initGameState :: GameState
initGameState = { fps: 0.0, rotating: false }

rotatingApple :: forall o. GlappleM GameState o (Picture Sprite)
rotatingApple = do
  timeMaybe <- getTotalTime
  { rotating } <- getGameState
  let
    time = case timeMaybe of
      Just (Milliseconds x) -> x
      Nothing -> 0.0
    revolution =
      if rotating then rotate (2.0 * pi * time / 1000.0)
      else identity
  pure $ sprite Apple
    # translate (-16.0) (-16.0)
    # scale 5.0 5.0
    # rotate (2.0 * pi * time / 200.0)
    # translate 80.0 80.0
    # revolution
    # translate 160.0 160.0

render :: forall o. GlappleM GameState o (Picture Sprite)
render = do
  { fps } <- getGameState
  apple <- rotatingApple
  let
    fpsText = translate 320.0 5.0 $ text
      (Fill (FillColor $ rgba' 1.0 0.0 0.0 1.0))
      AlignRight
      BaselineHanging
      fontStandard
      (show $ floor fps)

  pure $ fpsText <> apple

fontStandard :: Font
fontStandard = Font
  { fontFamily: Monospace
  , fontHeight: 40
  , fontSize: 20
  , fontWeight: Bold
  , fontStyle: FontStyleNormal
  }

gameSpec :: GameSpecM Sprite GameState Input Output
gameSpec = GameSpecM
  { canvasSpec:
      { width: 320.0
      , height: 320.0
      }
  , fps: 20
  , handler
  , initGameState
  , render
  , sprites
  }