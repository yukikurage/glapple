module Graphics.Glapple.Games.Particle where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Effect.Random (random)
import Graphics.Glapple (Event(..), GameId, GameSpecM(..), defaultHandler, destroy, emptyGameId, getGameState, modifyGameState, putGameState, renderGame, runGameWithM_)
import Graphics.Glapple.Data.Picture (Picture, opacity, rotate, translate)
import Graphics.GlappleEx.HOGs.Fixer (fixer)
import Graphics.GlappleEx.Utils (refTransform)
import Math (pi)

gameSpec
  :: forall s o i i'
   . { perSecond :: Number, life :: Number, spread :: Number, continue :: Maybe Number }
  -> Picture s
  -> GameSpecM s { waitTime :: Number, particles :: GameId s i', gameTime :: Number } i o
gameSpec { perSecond, life, spread, continue } pic = GameSpecM
  { eventHandler
  , inputHandler: defaultHandler
  , render
  , initGameState: do
      particles <- emptyGameId
      pure { waitTime: 0.0, particles, gameTime: 0.0 }
  }
  where
  eventHandler = case _ of
    Update { deltaTime } -> do
      { waitTime, particles, gameTime } <- getGameState
      putGameState { waitTime: waitTime + deltaTime, particles, gameTime: gameTime + deltaTime }
      case continue of
        Just x -> when (gameTime > x) destroy
        Nothing -> pure unit
    _ -> pure unit
  render = refTransform \t -> do
    { waitTime, particles } <- getGameState

    when (waitTime > 1.0 / perSecond) do
      modifyGameState _ { waitTime = 0.0 }
      r <- liftEffect random
      r' <- liftEffect random
      runGameWithM_
        ( fixer t $ gameSpecMonoParticle
            { direction: r * 2.0 * pi
            , angle: r' * 2.0 * pi
            , life
            , spread
            }
            pic
        ) $ particles

    pure $ renderGame particles

gameSpecMonoParticle
  :: forall s i o
   . { direction :: Number, angle :: Number, life :: Number, spread :: Number }
  -> Picture s
  -> GameSpecM s { o :: Number, x :: Number } i o
gameSpecMonoParticle { direction, angle, life, spread } pic = GameSpecM
  { eventHandler
  , inputHandler: defaultHandler
  , render
  , initGameState: pure { x: 0.0, o: 1.0 }
  }
  where
  eventHandler = case _ of
    Update { deltaTime } -> do
      modifyGameState \{ x, o } -> { x: x + deltaTime * spread / life, o: o - deltaTime / life }
    _ -> pure unit
  render = do
    { x, o } <- getGameState
    when (o < 0.0) destroy
    pure $ pic
      # rotate angle
      # translate x 0.0
      # rotate direction
      # opacity o
