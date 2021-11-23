module GlappleEx.HOGs.Fixer where

import Graphics.Canvas (Transform)
import Graphics.Glapple (GameSpecM(..))
import Graphics.Glapple.GlappleM (GlappleM(..))

-- | Fix the game with the given Transform
-- fixer :: forall s g i o. Transform -> GameSpecM s g i o -> GameSpecM s {transform :: Transform} i o
-- fixer t gameSpec = GameSpecM {}