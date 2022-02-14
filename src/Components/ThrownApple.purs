module Components.ThrownApple where

import Prelude

import Components.Particle (particle)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Graphics.Glapple.Data.Collider (Collider(..))
import Graphics.Glapple.Data.Component (Component)
import Graphics.Glapple.Hooks.UseClick (useClick)
import Graphics.Glapple.Hooks.UseDestroy (useDestroy)
import Graphics.Glapple.Hooks.UseFinalize (useFinalize)
import Graphics.Glapple.Hooks.UseRenderer (useRenderer)
import Graphics.Glapple.Hooks.UseRunner (useChildRunner)
import Graphics.Glapple.Hooks.UseTransform (useTranslate)
import Graphics.Glapple.Hooks.UseUpdate (useUpdate)
import Graphics.Glapple.Hooks.UseVelocity (useVelocity)
import Sprites (apple)

g :: Number
g = 500.0

thrownApple :: { x :: Number, y :: Number } -> Component Unit Unit
thrownApple initVelocity = do
  getVelocity /\ setVelocity <- useVelocity

  liftEffect $ setVelocity initVelocity

  destroy <- useDestroy
  getTrans /\ _ <- useTranslate

  useUpdate \{ deltaTime } -> do
    { x, y } <- getVelocity
    setVelocity { x, y: y + g * deltaTime }
    { y: h } <- getTrans
    when (h > 400.0) destroy

  useRenderer 0.0 $ pure apple

  useClick 0.0 (ColliderCircle 16.0) destroy

  runParticle /\ _ <- useChildRunner particle
  useFinalize $ runParticle { lifeTime: 1.0 }
