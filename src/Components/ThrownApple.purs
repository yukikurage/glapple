module Components.ThrownApple where

import Prelude

import Components.Particle (particle)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Graphics.Glapple.Data.Collider (Collider(..))
import Graphics.Glapple.Data.Complex (Complex, complex, image)
import Graphics.Glapple.Data.Component (Component(..))
import Graphics.Glapple.Hooks.UseClick (useClick)
import Graphics.Glapple.Hooks.UseDestroy (useDestroy)
import Graphics.Glapple.Hooks.UseFinalize (useFinalize)
import Graphics.Glapple.Hooks.UseRenderer (useRenderer)
import Graphics.Glapple.Hooks.UseRunner (useChildRunner)
import Graphics.Glapple.Hooks.UseTransform (useTranslate, useTranslateNow)
import Graphics.Glapple.Hooks.UseUpdate (useUpdate)
import Graphics.Glapple.Hooks.UseVelocity (useVelocity)
import Sprites (apple)

g :: Complex
g = complex 0.0 500.0

thrownApple
  :: forall r
   . Discard Unit
  => Component
       { initTranslate :: Complex
       , initVelocity :: Complex
       , onDestroy :: Effect Unit
       | r
       }
       Unit
       Unit
thrownApple = Component \{ initVelocity, initTranslate, onDestroy } -> do
  useTranslateNow initTranslate

  _ /\ setVelocity <- useVelocity

  liftEffect $ setVelocity initVelocity

  destroy <- useDestroy
  getTrans /\ _ <- useTranslate

  useUpdate \_ -> do
    trans <- getTrans
    when (image trans > 400.0) destroy

  useRenderer 0.0 $ pure apple

  useClick 0.0 (ColliderCircle 32.0) destroy

  particleRunner <- useChildRunner particle
  useFinalize $ do
    onDestroy
    particleRunner.run { lifeTime: 1.0 }
