module GlappleExamples.Games.Root where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Monad.Indexed.Qualified as Ix
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import GlappleExamples.Sprites (SpriteType(..))
import Graphics.Glapple.Data.Emitter (Emitter)
import Graphics.Glapple.Data.Picture (Picture, sprite)
import Graphics.Glapple.GlappleM (GlappleM, ilift)
import Graphics.Glapple.Runner (useRenderer)

root
  :: forall m r
   . MonadEffect m
  => GlappleM m
       ( rendererEmitter :: Emitter Unit (Picture SpriteType) m
       | r
       )
       ( rendererEmitter :: Emitter Unit (Picture SpriteType) m
       | r
       )
       Unit
root = Ix.do
  ilift $ log "added"
  _ <- useRenderer $ do
    pure $ sprite Apple
  ipure unit
