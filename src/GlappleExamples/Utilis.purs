module GlappleExamples.Utils where

import Prelude

import Data.Maybe (fromMaybe)
import Effect.Class (liftEffect)
import Graphics.Canvas (Transform)
import Graphics.Glapple.Data.Picture (Picture, absorb, drawWithTransform, empty)
import Graphics.Glapple.GlappleM (GlappleM, toEffect)

-- | Create a render referencing the current Transform.
refTransform :: forall s g i o. (Transform -> GlappleM s g i o (Picture s)) -> GlappleM s g i o (Picture s)
refTransform render = do
  renderEff <- toEffect render
  pure $ drawWithTransform \t -> absorb $ liftEffect do
    renderedMaybe <- renderEff t
    pure $ fromMaybe empty renderedMaybe