module GlappleExamples.HOGs.Fixer where

import Prelude

import Graphics.Canvas (Transform)
import Graphics.Glapple (Event(..), GameId, GameSpecM(..), destroy, getGameState, null, raise, renderGame, runChildGameM, tell)
import Graphics.Glapple.Data.Picture (absolute, transform)

-- | Fix the game with the given Transform
fixer :: forall s g i o. Transform -> GameSpecM s g i o -> GameSpecM s { gameId :: GameId s i } i o
fixer trans gameSpec = GameSpecM
  { initGameState: do
      gameId <- runChildGameM gameSpec \o -> raise o
      pure { gameId }
  , eventHandler: case _ of
      Update _ -> do
        { gameId } <- getGameState
        frag <- null gameId
        when frag destroy
      _ -> pure unit
  , inputHandler: \i -> do
      { gameId } <- getGameState
      tell gameId i
  , render: do
      { gameId } <- getGameState
      pure $ renderGame gameId # transform trans # absolute
  }