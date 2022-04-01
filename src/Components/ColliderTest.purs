module Components.ColliderTest where

import Prelude

import Data.Array (any)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Graphics.Glapple.Data.Collider (Collider(..), collide, filled, wireFrame)
import Graphics.Glapple.Data.Complex (complex)
import Graphics.Glapple.Data.Component (Component(..))
import Graphics.Glapple.Data.Transform (Transform, fromRotate, fromTranslate)
import Graphics.Glapple.Hooks.UseLocalTime (useLocalTime)
import Graphics.Glapple.Hooks.UseRenderer (useRenderer)
import Graphics.Glapple.Hooks.UseRunner (useChildRunnerNow)
import Graphics.Glapple.Hooks.UseTransform (useRotate, useTransform, useTranslate)
import Graphics.Glapple.Hooks.UseUpdate (useUpdate)
import Graphics.Glapple.UseMouseState (useMouseState)

mouseCollider :: Collider
mouseCollider = ColliderRect (complex 50.0 200.0)
  # ColliderTransform (fromTranslate (complex (-25.0) (-100.0)))

testCollider :: Collider
testCollider = ColliderRect (complex 80.0 160.0)

colliderTest :: forall input sprite. Component input sprite Unit
colliderTest = Component \_ -> do
  getTestTransform <- useChildRunnerNow testColliderComponent {}
  useChildRunnerNow mouse
    { getColliders: do
        tr <- getTestTransform
        pure [ ColliderTransform tr $ testCollider ]
    }

testColliderComponent
  :: forall input sprite. Component input sprite (Effect Transform)
testColliderComponent = Component \_ -> do
  getTransform <- useTransform
  _ /\ setTr <- useTranslate
  _ /\ setRot <- useRotate

  liftEffect $ setTr (complex 250.0 250.0)

  getTime <- useLocalTime

  useRenderer 0.0 do
    time <- getTime
    setRot time
    pure $ wireFrame testCollider

  pure getTransform

mouse
  :: forall sprite r
   . Component { getColliders :: Effect (Array Collider) | r } sprite Unit
mouse = Component \{ getColliders } -> do
  getTr /\ setTr <- useTranslate
  getRot /\ setRot <- useRotate

  getTime <- useLocalTime

  getMs <- useMouseState

  useUpdate \_ -> do
    ms <- getMs
    setTr ms
    time <- getTime
    pure unit
    setRot $ time * 1.7

  useRenderer 0.0 do
    tr <- getTr
    rot <- getRot
    colliders <- getColliders

    if
      any
        ( \col -> collide col
            ( ColliderTransform (fromTranslate tr)
                (ColliderTransform (fromRotate rot) mouseCollider)
            )
        )
        colliders then
      do
        pure $ filled mouseCollider
    else pure $ wireFrame mouseCollider
