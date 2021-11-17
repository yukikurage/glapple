module Graphics.Glapple.Data.GameSpecEffect (GameSpecEffect(..), runGame, CanvasSpec) where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Map (fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Data.Time (diff)
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, error, launchAff_, makeAff)
import Effect.Class (liftEffect)
import Effect.Now (nowTime)
import Effect.Ref (new, read, write)
import Graphic.Glapple.Data.Event (Event(..), KeyState(..))
import Graphic.Glapple.GlappleM (GlappleM, runGlappleM)
import Graphics.Canvas (CanvasElement, CanvasImageSource, clearRect, getContext2D, setCanvasHeight, setCanvasWidth, tryLoadImage)
import Graphics.Glapple.Data.GameId (GameId(..))
import Graphics.Glapple.Data.Picture (Picture, drawPicture)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent (key, repeat)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

type CanvasSpec =
  { height :: Number
  , width :: Number
  }

newtype GameSpecEffect sprite gameState input output = GameSpecEffect
  { fps :: Int
  , canvasSpec :: CanvasSpec
  , sprites :: Array (sprite /\ String)
  , initGameState :: gameState
  , render :: GlappleM gameState output (Picture sprite)
  , handler :: Event input -> GlappleM gameState output Unit
  }

-- | 画像の読み込み
tryLoadImageAff :: String -> Aff CanvasImageSource
tryLoadImageAff str = makeAff \thrower -> do
  tryLoadImage str $ case _ of
    Just x -> thrower $ Right x
    Nothing -> thrower $ Left $ error $ "Image LoadingError: " <> str
  pure mempty

runGame
  :: forall sprite gameState input output
   . Ord sprite
  => GameSpecEffect sprite gameState input output
  -> CanvasElement
  -> (output -> Effect Unit)
  -> Effect (GameId gameState input output)
runGame
  ( GameSpecEffect
      { fps
      , canvasSpec: { height, width }
      , sprites
      , initGameState
      , render
      , handler
      }
  )
  canvasElement
  outputHandler = do

  -- 様々なRefを定義
  gameStateRef <- new initGameState
  nowT <- nowTime
  deltaTimeRef <- new nowT
  initTimeRef <- new Nothing

  let
    internal = { outputHandler, initTimeRef, gameStateRef } --GlappleM内での状態

  launchAff_ do
    canvasImageSources <- loadImages
    initialize internal
    ctx <- liftEffect $ getContext2D canvasElement
    liftEffect do
      initTime <- nowTime
      write (Just initTime) initTimeRef
      write initTime deltaTimeRef
    forever $ mainProc ctx internal deltaTimeRef canvasImageSources :: Aff Unit

  pure $ GameId handler internal

  where

  initialize internal = liftEffect $ do
    setCanvasHeight canvasElement height
    setCanvasWidth canvasElement width

    w <- window
    let
      makeHandler x = eventListener \e -> case KeyboardEvent.fromEvent e of
        Just keyE | not (repeat keyE) -> do
          runGlappleM (handler (KeyEvent (key keyE) x)) internal
        _ -> pure unit
    keyDownHandler <- makeHandler KeyDown
    keyUpHandler <- makeHandler KeyUp
    addEventListener (EventType "keydown") keyDownHandler false $ Window.toEventTarget w
    addEventListener (EventType "keyup") keyUpHandler false $ Window.toEventTarget w

  loadImages = map fromFoldable
    $ for sprites (\(sprite /\ src) -> (sprite /\ _) <$> tryLoadImageAff src)

  mainProc ctx internal deltaTimeRef canvasImageSources = do
    procStart <- liftEffect nowTime
    liftEffect do
      picture <- runGlappleM render internal

      clearRect ctx { x: 0.0, y: 0.0, height, width }
      drawPicture ctx (\s -> lookup s canvasImageSources) picture

      nowT <- nowTime
      prevT <- read deltaTimeRef
      let
        d = diff nowT prevT
      write nowT deltaTimeRef
      runGlappleM (handler (Update d)) internal
    procEnd <- liftEffect nowTime
    let
      Milliseconds dt = diff procEnd procStart

    delay $ Milliseconds $ max 0.0 $ 1000.0 / toNumber fps - dt