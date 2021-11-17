module Graphics.Glapple.Data.GameSpecEffect where

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
import Graphics.Canvas (CanvasImageSource, clearRect, getContext2D, setCanvasHeight, setCanvasWidth, tryLoadImage)
import Graphics.Glapple.Data (GameId(..))
import Graphics.Glapple.Data.CanvasSpec (CanvasSpec(..))
import Graphics.Glapple.Data.Picture (Picture, drawPicture)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent (key, repeat)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

newtype GameSpecEffect sprite gameState input output = GameSpecEffect
  { fps :: Int
  , canvasSpec :: CanvasSpec
  , sprites :: Array (sprite /\ String)
  , initGameState :: gameState
  , render :: gameState -> GlappleM output (Picture sprite)
  , handler :: Event input -> gameState -> GlappleM output gameState
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
  -> (output -> Effect Unit)
  -> Effect (GameId gameState input output)
runGame
  ( GameSpecEffect
      { fps
      , canvasSpec: CanvasSpec { height, width, canvasElement }
      , sprites
      , initGameState
      , render
      , handler
      }
  )
  outputHandler = do
  gameStateRef <- new initGameState
  nowT <- nowTime
  deltaTimeRef <- new nowT
  initTimeRef <- new Nothing

  launchAff_ do
    canvasImageSources <- loadImages
    initialize gameStateRef initTimeRef
    ctx <- liftEffect $ getContext2D canvasElement
    liftEffect do
      initTime <- nowTime
      write (Just initTime) initTimeRef
      write initTime deltaTimeRef
    forever $ mainProc ctx gameStateRef initTimeRef deltaTimeRef canvasImageSources :: Aff Unit

  pure $ GameId handler { outputHandler, initTimeRef } gameStateRef

  where

  initialize gameStateRef initTimeRef = liftEffect $ do
    setCanvasHeight canvasElement height
    setCanvasWidth canvasElement width

    w <- window
    let
      makeHandler x = eventListener \e -> case KeyboardEvent.fromEvent e of
        Just keyE | not (repeat keyE) -> do
          gameState <- liftEffect $ read gameStateRef
          newGameState <- runGlappleM (handler (KeyEvent (key keyE) x) gameState) { outputHandler, initTimeRef }
          liftEffect $ write newGameState gameStateRef
        _ -> pure unit
    keyDownHandler <- makeHandler KeyDown
    keyUpHandler <- makeHandler KeyUp
    addEventListener (EventType "keydown") keyDownHandler false $ Window.toEventTarget w
    addEventListener (EventType "keyup") keyUpHandler false $ Window.toEventTarget w

  loadImages = map fromFoldable
    $ for sprites (\(sprite /\ src) -> (sprite /\ _) <$> tryLoadImageAff src)

  mainProc ctx gameStateRef initTimeRef deltaTimeRef canvasImageSources = do
    liftEffect do
      gameState <- read gameStateRef
      picture <- runGlappleM (render gameState) { outputHandler, initTimeRef }

      clearRect ctx { x: 0.0, y: 0.0, height, width }
      drawPicture ctx (\s -> lookup s canvasImageSources) picture

      nowT <- nowTime
      prevT <- read deltaTimeRef
      let
        d = diff nowT prevT
      write nowT deltaTimeRef
      newGameState <- runGlappleM (handler (Update d) gameState) { outputHandler, initTimeRef }
      liftEffect $ write newGameState gameStateRef

    delay $ Milliseconds $ 1000.0 / toNumber fps