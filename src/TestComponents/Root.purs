-- | ルートコンポーネント
module TestComponents.Root where

import Prelude

import Color (rgb', rgba')
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Graphics.Canvas (PatternRepeat(..), TextAlign(..), TextBaseline(..))
import Graphics.Glapple (Event(..), GameId, GameSpecM(..), KeyCode(..), getMousePosition, renderGame, runChildGameM_, tell)
import Graphics.Glapple.Data.Picture (DrawStyle(..), Font(..), FontFamily(..), FontStyle(..), FontWeight(..), Picture, Shape(..), arc, color, empty, fan, font, lineWidth, paint, polygon, rect, rotate, scale, sprite, text, textAlign, textBaseLine, translate, (<-*), (<-+), (<-.), (<-^))
import Graphics.Glapple.GlappleM (getGameState, modifyGameState)
import TestComponents.Apple as Apple
import TestComponents.DestroyTest as DestroyTest
import TestComponents.ParticleTest as ParticleTest
import TestComponents.Sprites (Sprite(..))

-- テスト用のゲーム
type GameState =
  { fps :: Number
  , rotating :: Boolean
  , apple :: GameId Sprite Apple.Input
  , destroyTest :: GameId Sprite Unit
  , particle :: GameId Sprite Unit
  }

type Input = Unit
type Output = Unit

gameSpec :: GameSpecM Sprite GameState Input Output
gameSpec = GameSpecM
  { eventHandler
  , inputHandler
  , initGameState
  , render
  }
  where
  inputHandler _ = do
    { apple } <- getGameState
    liftEffect $ tell apple $ Apple.StartRotate
    pure unit

  eventHandler = case _ of
    Update { deltaTime } -> modifyGameState $ \g@{ fps } -> g { fps = (1.0 / deltaTime) * 0.1 + fps * 0.9 }
    KeyEvent { keyCode: Keyboard "KeyX" } -> pure unit
    _ -> pure unit

  initGameState = do
    apple <- runChildGameM_ Apple.gameSpec
    destroyTest <- runChildGameM_ DestroyTest.gameSpec
    particle <- runChildGameM_ $ ParticleTest.gameSpec 30.0
      $ scale 0.5 0.5
      $ translate (-16.0) (-16.0)
      $ sprite Apple
    pure { fps: 0.0, rotating: false, apple, destroyTest, particle }

  render = do
    { apple, destroyTest, fps, particle } <- getGameState
    mousePos <- getMousePosition
    let
      fpsText = text Stroke (show $ floor fps)
        # textAlign AlignRight
        # textBaseLine BaselineHanging
        # font fontStandard
        # translate 320.0 5.0
        # color (rgba' 0.0 0.0 0.0 0.5)
        # lineWidth 1.0

    let
      particlePic = case mousePos of
        Just { mouseX, mouseY } -> translate mouseX mouseY $ renderGame particle
        Nothing -> empty

    pure $
      fpsText
        <-+ renderGame apple
        <-^ testPolygon
        <-. testPolygon2
        <-^ testPolygon3
        <-^ testPolygon4
        <-* testRect
        <-+ testArc
        <-+ testFan
        <-^ renderGame destroyTest
        <-^ particlePic
        <-^ renderGame destroyTest

testPolygon :: forall sprite. Picture sprite
testPolygon = polygon Fill polyData
  # paint
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
  # paint
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
  # paint (Pattern { sprite: Apple, repeat: Repeat })

testPolygon4 :: forall s. Picture s
testPolygon4 = polygon Stroke polyData
  # translate 0.0 160.0
  # color (rgb' 0.8 0.8 0.8)
  # lineWidth 10.0

testRect :: forall s. Picture s
testRect = color (rgb' 1.0 0.6 0.6) (rect Stroke 160.0 80.0)
  # rotate 0.6
  # translate 120.0 60.0
  # lineWidth 10.0

testArc :: forall s. Picture s
testArc =
  arc { start: 0.0, end: 1.6, radius: 80.0 }
    # translate 160.0 160.0
    # color (rgb' 1.0 0.5 1.0)
    # lineWidth 20.0

testFan :: Picture Sprite
testFan =
  fan Stroke { start: -1.3, end: 1.2, radius: 80.0 }
    # translate 120.0 120.0
    # paint (Pattern { sprite: Apple, repeat: Repeat })
    # lineWidth 20.0

fontStandard :: Font
fontStandard = Font
  { fontFamily: Monospace
  , fontHeight: 40
  , fontSize: 40
  , fontWeight: Bold
  , fontStyle: FontStyleNormal
  }
