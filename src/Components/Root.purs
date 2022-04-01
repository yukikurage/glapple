module Components.Root where

import Prelude

import Components.ThrownApple (thrownApple)
import Data.Number (infinity)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Graphics.Glapple.Data.Collider (Collider(..))
import Graphics.Glapple.Data.Complex (Complex, complex)
import Graphics.Glapple.Data.Component (Component(..))
import Graphics.Glapple.Data.Hooks (Hooks)
import Graphics.Glapple.Hooks.UseHover (useHover)
import Graphics.Glapple.Hooks.UseRunner (useChildRunner)
import Graphics.Glapple.Hooks.UseState (useState)
import Graphics.Glapple.Hooks.UseUpdate (useUpdate)

startPoint :: Complex
startPoint = complex 10.0 10.0

-- | 球を投げる場所
root :: forall input. Component input Unit Unit
root = Component \_ -> do
  appleRunner <- useChildRunner thrownApple
  getIsDestroy /\ setIsDestroy <- useState false

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

-- prevent -- 操作不能にする

prevent :: forall t57. Hooks t57 Unit
prevent = do
  _ <- useHover (-infinity) $ ColliderRect $ complex 500.0 500.0
  pure unit
