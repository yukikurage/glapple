module Main where

import Prelude

import Color (rgb', rgba')
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (delay, runAff_)
import Effect.Class (liftEffect)
import Graphic.Glapple.Data.Event (Event(..))
import Graphic.Glapple.GlappleM (GlappleM, getGameState, getTotalTime)
import Graphics.Canvas (PatternRepeat(..), TextAlign(..), TextBaseline(..), getCanvasElementById)
import Graphics.Glapple.Data.GameId (tell)
import Graphics.Glapple.Data.GameSpec (mkHandlerM)
import Graphics.Glapple.Data.GameSpecM (GameSpecM(..), runGameM_)
import Graphics.Glapple.Data.Picture (DrawStyle(..), Font(..), FontFamily(..), FontStyle(..), FontWeight(..), Picture, ShapeStyle(..), arc, color, draw, fan, polygon, rect, rotate, scale, sprite, text, translate, (<-*), (<-+), (<-.), (<-^))
import Math (pi)

main :: Effect Unit
main = do
  canvasMaybe <- getCanvasElementById "canvas"
  case canvasMaybe of
    Nothing -> pure unit
    Just canvas -> do
      gameId <- runGameM_ gameSpec canvas
      runAff_ (const $ pure unit) do
        delay $ Milliseconds 3000.0
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
handler = mkHandlerM \e gameState -> case e of
  Update (Milliseconds t) -> gameState { fps = 1000.0 / t }
  Input _ -> gameState { rotating = true }
  _ -> gameState

initGameState :: GameState
initGameState = { fps: 0.0, rotating: false }

testPolygon :: forall sprite. Picture sprite
testPolygon = polygon Fill polyData
  # draw
      ( LinearGradient
          { x0: 0.0
          , x1: 200.0
          , y0: 0.0
          , y1: 200.0
          , colorStops: [ 0.0 /\ rgb' 1.0 0.5 0.0, 1.0 /\ rgb' 0.0 0.5 1.0 ]
          }
      )

polyData :: Array (Tuple Number Number)
polyData = [ 10.0 /\ 5.0, 180.0 /\ 60.0, 64.0 /\ 256.0 ]

testPolygon2 :: forall s. Picture s
testPolygon2 = polygon Fill polyData
  # translate 140.0 0.0
  # draw
      ( RadialGradient
          { x0: 160.0
          , x1: 160.0
          , r0: 10.0
          , y0: 160.0
          , y1: 160.0
          , r1: 160.0
          , colorStops: [ 0.0 /\ rgb' 1.0 0.0 0.0, 1.0 /\ rgb' 0.0 0.0 1.0 ]
          }
      )

testPolygon3 :: Picture Sprite
testPolygon3 = polygon Fill polyData
  # translate 80.0 150.0
  # draw (Pattern { sprite: Apple, repeat: Repeat })

testPolygon4 :: forall s. Picture s
testPolygon4 = polygon (Stroke { lineWidth: 10.0 }) polyData
  # translate 0.0 160.0
  # color (rgb' 0.8 0.8 0.8)

testRect :: forall s. Picture s
testRect = color (rgb' 1.0 0.6 0.6) (rect (Stroke $ { lineWidth: 10.0 }) 160.0 80.0)
  # rotate 0.6
  # translate 120.0 60.0

testArc :: forall s. Picture s
testArc =
  arc { lineWidth: 20.0 } { start: 0.0, end: 1.6, radius: 80.0 }
    # translate 160.0 160.0
    # color (rgb' 1.0 0.5 1.0)

testFan :: Picture Sprite
testFan =
  fan (Stroke { lineWidth: 20.0 }) { start: -1.3, end: 1.2, radius: 80.0 }
    # translate 120.0 120.0
    # draw (Pattern { sprite: Apple, repeat: Repeat })

rotatingApple :: forall o. GlappleM GameState o (Picture Sprite)
rotatingApple = do
  timeMaybe <- getTotalTime
  { rotating } <- getGameState
  let
    time = case timeMaybe of
      Just (Milliseconds x) -> x
      Nothing -> 0.0
    revolution =
      if rotating then rotate (2.0 * pi * time / 400.0)
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
    fpsText = color (rgba' 1.0 0.0 0.0 1.0) $ translate 320.0 5.0 $ text
      Fill
      AlignRight
      BaselineHanging
      fontStandard
      (show $ floor fps)

  pure $
    fpsText
      <-+ apple
      <-^ testPolygon
      <-. testPolygon2
      <-^ testPolygon3
      <-^ testPolygon4
      <-* testRect
      <-+ testArc
      <-+ testFan

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
  , fps: 60
  , handler
  , initGameState
  , render
  , sprites
  }