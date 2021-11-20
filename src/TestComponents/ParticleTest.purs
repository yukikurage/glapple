module TestComponents.ParticleTest where

import Prelude

import Effect.Class (liftEffect)
import Effect.Random (random)
import Graphics.Glapple (Event(..), GameSpecM(..), defaultHandler, defaultRender, destroyMe, getGameState, modifyGameState, putGameState, runChildGameM_)
import Graphics.Glapple.Data.Picture (Picture, opacity, rotate, translate)
import Math (pi)

gameSpec
  :: forall s i o
   . Number
  -> Picture s
  -> GameSpecM s { waitTime :: Number } i o
gameSpec pps pic = GameSpecM
  { eventHandler
  , inputHandler: defaultHandler
  , render: defaultRender
  , initGameState: pure { waitTime: 0.0 }
  }
  where
  eventHandler = case _ of
    Update { deltaTime } -> do
      { waitTime } <- getGameState
      when (waitTime > 1.0 / pps) do
        r <- liftEffect random
        _ <- runChildGameM_ $ gameSpecMonoParticle (r * 2.0 * pi) pic
        pure unit
      putGameState { waitTime: waitTime + deltaTime }
    _ -> pure unit

-- | パーティクル1つ
gameSpecMonoParticle
  :: forall s i o
   . Number
  -> Picture s
  -> GameSpecM s { o :: Number, x :: Number } i o
gameSpecMonoParticle r pic = GameSpecM
  { eventHandler
  , inputHandler: defaultHandler
  , render
  , initGameState: pure { x: 0.0, o: 1.0 }
  }
  where
  eventHandler = case _ of
    Update { deltaTime } -> do
      modifyGameState \{ x, o } -> { x: x + deltaTime, o: o - deltaTime }
    _ -> pure unit
  render = do
    { x, o } <- getGameState
    when (o < 0.0) destroyMe
    pure $ pic
      # translate x 0.0
      # rotate r
      # opacity o
