module Components.Root where

import Prelude

import Color.Scheme.Clrs (purple, red)
import Components.ColliderTest (colliderTest)
import Components.ThrownApple (thrownApple)
import Data.Number (infinity)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Graphics.Canvas (PatternRepeat(..))
import Graphics.Glapple.Data.Collider (Collider(..))
import Graphics.Glapple.Data.Complex (Complex, complex)
import Graphics.Glapple.Data.Component (Component(..))
import Graphics.Glapple.Data.Hooks (Hooks)
import Graphics.Glapple.Data.Picture (DrawStyle(..), Picture, Shape(..), paint, polygon, translate)
import Graphics.Glapple.Hooks.UseHover (useHover)
import Graphics.Glapple.Hooks.UseRenderer (useRenderer)
import Graphics.Glapple.Hooks.UseRunner (useChildRunner, useRunnerNow)
import Graphics.Glapple.Hooks.UseState (useState)
import Graphics.Glapple.Hooks.UseUpdate (useUpdate)

startPoint :: Complex
startPoint = complex 10.0 10.0

-- | 球を投げる場所
root :: forall input. Component input Unit Unit
root = Component \_ -> do
  appleRunner <- useChildRunner thrownApple
  getIsDestroy /\ setIsDestroy <- useState false

  useRunnerNow colliderTest {}

  useRenderer 0.0 $ pure polygonTest

  useRenderer (-1.0) $ pure polygonTest2

  liftEffect $ appleRunner.run
    { initTranslate: complex 250.0 250.0
    , initVelocity: complex 100.0 100.0
    , onDestroy: setIsDestroy true
    }

  useUpdate \_ -> do
    isDestroy <- getIsDestroy
    when isDestroy $ appleRunner.run
      { initTranslate: complex 250.0 250.0
      , initVelocity: complex 100.0 100.0
      , onDestroy: setIsDestroy true
      }
    setIsDestroy false

prevent :: forall t57. Hooks t57 Unit
prevent = do
  _ <- useHover (-infinity) $ ColliderRect $ complex 500.0 500.0
  pure unit

polygonTest :: Picture Unit
polygonTest =
  polygon Fill
    [ complex 0.0 0.0, complex 120.0 80.0, complex 30.0 90.0 ]
    # paint (Pattern { sprite: unit, repeat: Repeat })

polygonTest2 :: Picture Unit
polygonTest2 =
  polygon Fill
    [ complex 0.0 0.0, complex 120.0 80.0, complex 30.0 90.0 ]
    # paint
        ( LinearGradient
            { x0: 0.0
            , y0: 0.0
            , x1: 120.0
            , y1: 120.0
            , colorStops: [ 0.0 /\ purple, 1.0 /\ red ]
            }
        )
    # translate (complex 100.0 100.0)
