module TestComponents.ParticleTest where

import Prelude

import Effect.Class (liftEffect)
import Effect.Random (random)
import Graphics.Canvas (Transform)
import Graphics.Glapple (Event(..), GameId, GameSpecM(..), defaultHandler, destroy, getGameState, modifyGameState, putGameState, renderGame, runGameWithM_)
import Graphics.Glapple.Data.GameId (emptyGameId)
import Graphics.Glapple.Data.Picture (Picture, absolute, opacity, rotate, transform, translate)
import Graphics.GlappleEx.RefTransform (refTransform)
import Math (pi)

gameSpec
  :: forall s o i i'
   . Number
  -> Picture s
  -> GameSpecM s { waitTime :: Number, particles :: GameId s i' } i o
gameSpec pps pic = GameSpecM
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
    _ -> pure unit
  render = refTransform \t -> do
    { waitTime, particles } <- getGameState
    when (waitTime > 1.0 / pps) do
      modifyGameState _ { waitTime = 0.0 }
      r <- liftEffect random
      runGameWithM_ (gameSpecMonoParticle t (r * 2.0 * pi) pic) $ particles
    pure $ renderGame particles

-- | パーティクル1つ
gameSpecMonoParticle
  :: forall s i o
   . Transform
  -> Number
  -> Picture s
  -> GameSpecM s { o :: Number, x :: Number } i o
gameSpecMonoParticle t r pic = GameSpecM
  { eventHandler
  , inputHandler: defaultHandler
  , render
  , initGameState: pure { x: 0.0, o: 1.0 }
  }
  where
  eventHandler = case _ of
    Update { deltaTime } -> do
      modifyGameState \{ x, o } -> { x: x + deltaTime * 50.0, o: o - deltaTime }
    _ -> pure unit
  render = do
    { x, o } <- getGameState
    when (o < 0.0) destroy
    pure $ pic
      # translate x 0.0
      # rotate r
      # transform t
      # opacity o
      # absolute
