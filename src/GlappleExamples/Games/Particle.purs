module Graphics.GlappleEx.Games.Particle where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Effect.Class (liftEffect)
import Effect.Random (random)
import Graphics.Glapple (Event(..), GameId, GameSpecM(..), defaultHandler, destroy, emptyGameId, getGameState, getLocalTime, modifyGameState, putGameState, renderGame, runGameWithM_, null)
import Graphics.Glapple.Data.Picture (Picture, opacity, rotate, translate)
import GlappleExamples.HOGs.Fixer (fixer)
import GlappleExamples.Utils (refTransform)
import Math (pi)

gameSpec
  :: forall s o i i'
   . { perSecond :: Number, life :: Number, spread :: Number, continue :: Maybe Number }
  -> Picture s
  -> GameSpecM s { waitTime :: Number, particles :: GameId s i' } i o
gameSpec { perSecond, life, spread, continue } pic = GameSpecM
  { eventHandler
  , inputHandler: defaultHandler
  , render
  , initGameState: do
      particles <- emptyGameId
      pure { waitTime: 0.0, particles }
  }
  where
  eventHandler = case _ of
    Update { deltaTime } -> do
      { waitTime, particles } <- getGameState
      putGameState { waitTime: waitTime + deltaTime, particles }
      localTime <- getLocalTime
      frag <- null particles
      when (frag && fromMaybe false ((\c -> localTime > c) <$> continue)) destroy
    _ -> pure unit
  render = refTransform \t -> do
    { waitTime, particles } <- getGameState
    localTime <- getLocalTime

    when (waitTime > 1.0 / perSecond && fromMaybe true ((\c -> localTime < c) <$> continue)) do
      modifyGameState _ { waitTime = 0.0 }
      r <- liftEffect random
      r' <- liftEffect random
      runGameWithM_ particles $ fixer t $ gameSpecMonoParticle
        { direction: r * 2.0 * pi
        , angle: r' * 2.0 * pi
        , life
        , spread
        }
        pic

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
