module Graphics.Glapple.Data where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (Ref, read, write)
import Graphic.Glapple.Data.Event (Event(..))
import Graphic.Glapple.GlappleM (GlappleM, InternalState, runGlappleM)

data GameId gameState input output =
  GameId (Event input -> gameState -> GlappleM output gameState) (InternalState output) (Ref gameState)

tell :: forall gameState input output. GameId gameState input output -> input -> Effect Unit
tell (GameId handler internal ref) input = do
  gameState <- read ref
  newGameState <- runGlappleM (handler (Input input) gameState) internal
  liftEffect $ write newGameState ref