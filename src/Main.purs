module Main where

import Prelude

import Color (rgb)
import Data.Int (toNumber)
import Data.Maybe (maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Graphics.Canvas (TextBaseline(..), getCanvasElementById)
import Graphics.Glapple (Event(..), GameId(..), GameSpecM(..), KeyCode(..), KeyState(..), defaultHandler, destroy, getGameState, modifyGameState, putGameState, raise, renderGame, runChildGameM, runGameM, runGameM_, tell)
import Graphics.Glapple.Data.Picture (Font(..), FontFamily(..), FontStyle(..), FontWeight(..), Shape(..), arc, color, font, lineWidth, polygon, rect, rotate, sprite, text, textBaseLine, translate, (<-^))
import Graphics.Glapple.Data.SpriteData (SpriteData(..))
import Math (pi)

data Sprite = Hoge | Fuga

derive instance Eq Sprite
derive instance Ord Sprite

main :: Effect Unit
main = do
  canvas <- maybe (throw "Can not find canvas element") pure
    =<< getCanvasElementById "canvas"

  let
    outputHandler = \o -> log o
  _ <- runGameM 60.0 canvas { width: 320.0, height: 320.0 } [] gameSpec outputHandler
  pure unit

gameSpec :: GameSpecM Sprite { steps :: Int, gameId :: GameId Sprite Int } Unit String
gameSpec = GameSpecM
  { initGameState: do
      let
        outputHandler = \n -> do
          { steps } <- getGameState
          log $ "Parent Game's step is " <> show steps
          log $ "Child Game's step is " <> show n
      gameId <- runChildGameM gameSpec2 outputHandler
      pure { steps: 0, gameId }
  , eventHandler: case _ of
      KeyEvent { keyCode: Keyboard "KeyA", keyState: KeyDown } -> do
        { steps, gameId } <- getGameState
        modifyGameState _ { steps = steps + 1 }
        tell gameId steps
      _ -> pure unit
  , inputHandler: defaultHandler
  , render: do
      { steps, gameId } <- getGameState
      pure $ renderGame gameId <-^ rect Fill 20.0 20.0
        # translate (toNumber steps * 10.0) 0.0
  }

gameSpec2 :: GameSpecM Sprite Int Int Int
gameSpec2 = GameSpecM
  { initGameState: pure 10
  , eventHandler: case _ of
      KeyEvent { keyCode: Keyboard "KeyB", keyState: KeyDown } -> do
        modifyGameState (_ - 1)
      _ -> pure unit
  , inputHandler: \n -> do
      steps <- getGameState
      when (n >= 5) do
        raise steps
        destroy
  , render: do
      steps <- getGameState
      pure $ rect Fill 20.0 20.0
        # translate 0.0 (toNumber steps * 10.0)
  }

poly1 = polygon Fill [ 300.0 /\ 200.0, 100.0 /\ 200.0, 0.0 /\ 30.0 ]
  # color (rgb 225 0 225)

poly2 = polygon Fill [ 100.0 /\ 100.0, 200.0 /\ 100.0, 20.0 /\ 30.0 ]
  # color (rgb 0 0 225)

testFont :: Font
testFont = Font
  { fontFamily: Monospace
  , fontHeight: 40
  , fontSize: 40
  , fontWeight: Bold
  , fontStyle: FontStyleNormal
  }

-- case canvasMaybe of
--   Nothing -> pure unit
--   Just canvas -> do
--     gameId <- runGameM_ 60.0 canvas { width: 320.0, height: 320.0 } sprites gameSpec
--     runAff_ (const $ pure unit) do
--       delay $ Milliseconds 3000.0
--       liftEffect $ tell gameId $ unit
