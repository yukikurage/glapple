module Components.UseFps where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Graphics.Glapple.Data.Hooks (Hooks)
import Graphics.Glapple.Hooks.UseState (useState)
import Graphics.Glapple.Hooks.UseUpdate (useUpdate)

useFps :: forall sprite. Hooks sprite (Effect Number)
useFps = do
  getFps /\ setFps <- useState 0.0

  useUpdate \{ deltaTime } -> do
    fps <- getFps
    when (deltaTime /= 0.0) $ setFps $ (9.0 * fps + (1.0 / deltaTime)) / 10.0

  pure getFps
