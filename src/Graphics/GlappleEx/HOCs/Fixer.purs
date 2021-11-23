module Graphics.GlappleEx.HOGs.Fixer where

import Prelude

import Graphics.Canvas (Transform)
import Graphics.Glapple (GameId, GameSpecM(..), defaultHandler, getGameState, raise, renderGame, runChildGameM, tell)
import Graphics.Glapple.Data.Picture (absolute, transform)

-- | Fix the game with the given Transform
fixer :: forall s g i o. Transform -> GameSpecM s g i o -> GameSpecM s { gameId :: GameId s i } i o
fixer trans gameSpec = GameSpecM
  { initGameState: do
      gameId <- runChildGameM gameSpec \o -> raise o
      pure { gameId }
  , eventHandler: defaultHandler
  , inputHandler: \i -> do
      { gameId } <- getGameState
      tell gameId i
  , render: do
      { gameId } <- getGameState
      pure $ renderGame gameId # transform trans # absolute
  }