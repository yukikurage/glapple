module TestComponents.ParticleTest where

import Prelude

import Effect.Class (liftEffect)
import Effect.Random (random)
import Graphics.Glapple (Event(..), GameSlot, GameSpecM(..), defaultHandler, destroyMe, getGameState, modifyGameState, putGameState, runGameSlot_)
import Graphics.Glapple.Data.GameSlot (emptyGameSlot, renderGameSlot)
import Graphics.Glapple.Data.Picture (Picture, opacity, rotate, translate)
import Math (pi)

type Input = { x :: Number, y :: Number }

gameSpec
  :: forall s o i'
   . Number
  -> Picture s
  -> GameSpecM s { x :: Number, y :: Number, waitTime :: Number, particles :: GameSlot s i' } Input o
gameSpec pps pic = GameSpecM
  { eventHandler
  , inputHandler
  , render
  , initGameState: do
      particles <- emptyGameSlot
      pure { waitTime: 0.0, particles, x: 0.0, y: 0.0 }
  }
  where
  eventHandler = case _ of
    Update { deltaTime } -> do
      { x, y, waitTime, particles } <- getGameState
      if (waitTime > 1.0 / pps) then do
        modifyGameState _ { waitTime = 0.0 }
        r <- liftEffect random
        runGameSlot_ (gameSpecMonoParticle { ix: x, iy: y } (r * 2.0 * pi) pic) $ particles
        pure unit
      else putGameState { x, y, waitTime: waitTime + deltaTime, particles }
    _ -> pure unit
  inputHandler = case _ of
    { x, y } -> modifyGameState _ { x = x, y = y }
  render = do
    { particles } <- getGameState
    pure $ renderGameSlot particles

-- | パーティクル1つ
gameSpecMonoParticle
  :: forall s i o
   . { ix :: Number, iy :: Number }
  -> Number
  -> Picture s
  -> GameSpecM s { o :: Number, x :: Number } i o
gameSpecMonoParticle { ix, iy } r pic = GameSpecM
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
    when (o < 0.0) destroyMe
    pure $ pic
      # translate x 0.0
      # rotate r
      # translate ix iy
      # opacity o
