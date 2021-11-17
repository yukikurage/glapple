module Graphics.Glapple.Data.GameId where

import Prelude

import Effect (Effect)
import Graphic.Glapple.Data.Event (Event(..))
import Graphic.Glapple.GlappleM (GlappleM, InternalState, runGlappleM)

data GameId gameState input output =
  GameId (Event input -> GlappleM gameState output Unit)
    (InternalState gameState output)

tell :: forall gameState input output. GameId gameState input output -> input -> Effect Unit
tell (GameId handler internal) input = do
  runGlappleM (handler (Input input)) internal