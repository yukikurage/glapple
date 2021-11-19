module Graphics.Glapple.Data.InternalRegistrationIds where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Graphics.Canvas (CanvasImageSource, Context2D)
import Graphics.Glapple.Data.Emitter (RegistrationId, unregister)
import Graphics.Glapple.Data.Event (Event)

-- | それぞれのゲームが内部に持っているRegistrationIdの値
-- | ゲームdestroy時に全部unregister
type InternalRegistrationIds s i o =
  { inputId :: RegistrationId Effect i
  , renderId :: RegistrationId Aff { context2D :: Context2D, canvasImageSources :: s -> Maybe CanvasImageSource }
  , outputId :: RegistrationId Effect o
  , eventId :: RegistrationId Effect Event
  }

-- | InternalRegistrationIdsをとってゲームを処理から切り離す
unregisterGame :: forall s i o m. MonadEffect m => InternalRegistrationIds s i o -> m Unit
unregisterGame { inputId, renderId, outputId, eventId } = do
  unregister inputId
  unregister renderId
  unregister outputId
  unregister eventId