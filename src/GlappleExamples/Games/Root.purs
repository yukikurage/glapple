module GlappleExamples.Games.Root where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import GlappleExamples.Sprites (SpriteType(..))
import Graphics.Canvas (Transform)
import Graphics.Glapple.Data.Hooks (component)
import Graphics.Glapple.Data.Hooks.Qualified as H
import Graphics.Glapple.Data.Picture (sprite, transform, translate)
import Graphics.Glapple.Hooks.GameComponent (GameComponent)
import Graphics.Glapple.Hooks.UseLauncher (useLauncher)
import Graphics.Glapple.Hooks.UseRenderer (useRenderer)
import Graphics.Glapple.Hooks.UseTimeOut (useTimeOut)

root
  :: forall m props contexts
   . MonadEffect m
  => GameComponent SpriteType contexts props m Unit
root = component \_ -> H.do
  launcher /\ destroyer <- useLauncher child

  useTimeOut 2.0 do
    _ <- launcher $ translate 50.0 50.0
    pure unit

  useTimeOut 4.0 do
    _ <- launcher $ translate 100.0 100.0
    pure unit

  useTimeOut 6.0 do
    _ <- launcher $ translate 150.0 150.0
    pure unit

  useTimeOut 8.0 destroyer

  useRenderer $ \_ -> do
    pure $ sprite Apple

child
  :: forall m contexts
   . MonadEffect m
  => GameComponent SpriteType contexts Transform m Unit
child = component \trans -> H.do
  -- destroy <- useDestroyer

  -- useTimeOut 2.0 do
  --   destroy
  --   pure unit

  useRenderer $ \_ -> do
    pure $ sprite Apple # transform trans
