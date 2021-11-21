-- | runGameMでゲームを実行します．
module Graphics.Glapple.GameRunnerM (runChildGameM, runChildGameM_, runGameM, runGameM_, runGameWithM, runGameWithM_, createCanvasElement) where

import Prelude

import Control.Monad.Reader (ask)
import Control.Monad.Rec.Class (forever)
import Data.Int (toNumber)
import Data.Map (fromFoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (delete, insert)
import Data.Time (diff)
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (nowTime)
import Effect.Ref (modify_, new, read, write)
import Graphics.Canvas (CanvasElement, CanvasImageSource, Context2D, canvasElementToImageSource, clearRect, drawImage, getContext2D, setCanvasHeight, setCanvasWidth)
import Graphics.Glapple.Data.Emitter (fire, newEmitter, register)
import Graphics.Glapple.Data.Event (Event(..), KeyCode(..), KeyState(..), MouseButton(..))
import Graphics.Glapple.Data.GameId (GameId(..))
import Graphics.Glapple.Data.GameSpecM (CanvasSpec, GameSpecM(..))
import Graphics.Glapple.Data.Picture (Picture, drawPicture, empty, tryLoadImageAff)
import Graphics.Glapple.GlappleM (GlappleM, InternalState, runGlappleM)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (HTMLCanvasElement, window)
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Web.HTML.HTMLElement (getBoundingClientRect)
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent (code, repeat)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.KeyboardEvent.EventTypes (keydown, keyup)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.MouseEvent.EventTypes (mousedown, mousemove, mouseup)

--------------------
-- Run Child Game --
--------------------

foreign import createCanvasElement :: Effect CanvasElement

makeRenderHandler
  :: forall s g i o
   . InternalState s g i o
  -> GlappleM s g i o (Picture s)
  -> ({ context2D :: Context2D, canvasImageSources :: s -> Maybe CanvasImageSource } -> Aff Unit)
makeRenderHandler internalState render = \{ context2D, canvasImageSources } -> do --ここ以下がレンダリング毎に実行される
  pic <- liftEffect $ map (fromMaybe empty) $ runGlappleM render internalState
  drawPicture context2D canvasImageSources pic

makeHandlerEffect
  :: forall s g i o a
   . InternalState s g i o
  -> (a -> GlappleM s g i o Unit)
  -> (a -> Effect Unit)
makeHandlerEffect internalState eventHandler = \e -> map (fromMaybe unit)
  $ runGlappleM (eventHandler e) internalState

-- | 現在のゲームの中で，新しく子ゲームを作る
runChildGameM
  :: forall s g i o childG childI childO
   . GameSpecM s childG childI childO
  -> (childO -> GlappleM s g i o Unit)
  -> GlappleM s g i o (GameId s childI)
runChildGameM (GameSpecM { initGameState, render, eventHandler, inputHandler }) outputHandler = do
  internalState@{ eventEmitter, initTimeRef, keyStateRef, mousePositionRef } <- ask
  gameStateRef <- liftEffect $ new Nothing
  internalRegistrationIdsRef <- liftEffect $ new Nothing

  inputEmitter <- newEmitter
  outputEmitter <- newEmitter
  renderEmitter <- newEmitter

  let
    childInternalState =
      { eventEmitter
      , outputEmitter
      , initTimeRef
      , gameStateRef
      , internalRegistrationIdsRef
      , keyStateRef
      , mousePositionRef
      }

  let
    inputHandler_ = makeHandlerEffect childInternalState inputHandler
    renderHandler_ = makeRenderHandler childInternalState render
    eventHandler_ = makeHandlerEffect childInternalState eventHandler
    outputHandler_ = makeHandlerEffect internalState outputHandler

  inputId <- register inputEmitter inputHandler_
  outputId <- register outputEmitter outputHandler_
  renderId <- register renderEmitter renderHandler_
  eventId <- register eventEmitter eventHandler_

  let
    internalRegistrationIds = { inputId, outputId, eventId, renderId }
    gameId = GameId { inputEmitter, renderEmitter }

  liftEffect $ write (Just internalRegistrationIds) internalRegistrationIdsRef

  _ <- liftEffect $ flip runGlappleM childInternalState do
    gameState <- initGameState
    liftEffect $ write (Just gameState) gameStateRef

  pure gameId

runChildGameM_
  :: forall s g i o childG childI childO
   . GameSpecM s childG childI childO
  -> GlappleM s g i o (GameId s childI)
runChildGameM_ gameSpecM = runChildGameM gameSpecM \_ -> pure unit

-------------------
-- Run Game With --
-------------------
-- | GameSlotにGameを追加する
runGameWithM
  :: forall s g i o childG childI childO
   . GameSpecM s childG childI childO
  -> GameId s childI
  -> (childO -> GlappleM s g i o Unit)
  -> GlappleM s g i o Unit
runGameWithM (GameSpecM { initGameState, render, eventHandler, inputHandler }) (GameId { inputEmitter, renderEmitter }) outputHandler = do
  internalState@{ eventEmitter, initTimeRef, keyStateRef, mousePositionRef } <- ask
  gameStateRef <- liftEffect $ new Nothing
  internalRegistrationIdsRef <- liftEffect $ new Nothing

  outputEmitter <- newEmitter

  let
    childInternalState =
      { eventEmitter
      , outputEmitter
      , initTimeRef
      , gameStateRef
      , internalRegistrationIdsRef
      , keyStateRef
      , mousePositionRef
      }

  let
    inputHandler_ = makeHandlerEffect childInternalState inputHandler
    renderHandler_ = makeRenderHandler childInternalState render
    eventHandler_ = makeHandlerEffect childInternalState eventHandler
    outputHandler_ = makeHandlerEffect internalState outputHandler

  inputId <- register inputEmitter inputHandler_
  outputId <- register outputEmitter outputHandler_
  renderId <- register renderEmitter renderHandler_
  eventId <- register eventEmitter eventHandler_

  let
    internalRegistrationIds = { inputId, outputId, eventId, renderId }

  liftEffect $ write (Just internalRegistrationIds) internalRegistrationIdsRef

  _ <- liftEffect $ flip runGlappleM childInternalState do
    gameState <- initGameState
    liftEffect $ write (Just gameState) gameStateRef

  pure unit

runGameWithM_
  :: forall s g i o childG childI childO
   . GameSpecM s childG childI childO
  -> GameId s childI
  -> GlappleM s g i o Unit
runGameWithM_ gameSpecM gameSlot = runGameWithM gameSpecM gameSlot \_ -> pure unit

--------------
-- Run Game --
-- -----------

loadImages :: forall s. Ord s => Array (s /\ String) -> Aff (s -> Maybe CanvasImageSource)
loadImages sprites = do
  tmp <- map fromFoldable
    $ for sprites (\(sprite /\ src) -> (sprite /\ _) <$> tryLoadImageAff src)
  pure \s -> lookup s tmp

-- | トップレベルでゲームを作る
runGameM
  :: forall s g i o
   . Ord s
  => Number
  -> CanvasElement
  -> CanvasSpec
  -> Array (s /\ String)
  -> GameSpecM s g i o
  -> (o -> Effect Unit)
  -> Effect (GameId s i)

runGameM
  fps
  canvasElement
  { height, width }
  sprites
  (GameSpecM { initGameState, render, eventHandler, inputHandler })
  outputHandler = do

  -- 様々なRefを定義
  gameStateRef <- new Nothing
  initTimeRef <- new Nothing
  internalRegistrationIdsRef <- new Nothing
  keyStateRef <- new mempty
  mousePositionRef <- new { mouseX: 0.0, mouseY: 0.0 }

  -- Emitterを作成
  inputEmitter <- newEmitter
  outputEmitter <- newEmitter
  renderEmitter <- newEmitter
  eventEmitter <- newEmitter

  let
    internalState =
      { eventEmitter
      , outputEmitter
      , initTimeRef
      , gameStateRef
      , internalRegistrationIdsRef
      , keyStateRef
      , mousePositionRef
      }

    inputHandler_ = makeHandlerEffect internalState inputHandler
    renderHandler_ = makeRenderHandler internalState render
    eventHandler_ = makeHandlerEffect internalState eventHandler

  inputId <- register inputEmitter inputHandler_
  outputId <- register outputEmitter outputHandler
  renderId <- register renderEmitter renderHandler_
  eventId <- register eventEmitter eventHandler_

  let
    internalRegistrationIds = { inputId, outputId, renderId, eventId }
    gameId = GameId { inputEmitter, renderEmitter }

  write (Just internalRegistrationIds) internalRegistrationIdsRef

  -- キャンバス系
  offCanvas <- createCanvasElement --裏画面
  offContext2D <- getContext2D offCanvas --裏画面のcontext2D
  context2D <- getContext2D canvasElement
  setCanvasHeight offCanvas height
  setCanvasWidth offCanvas width
  setCanvasHeight canvasElement height
  setCanvasWidth canvasElement width

  -- Windowを取得
  w <- Window.toEventTarget <$> window

  -- Web EventでEmitterを発火させる
  keyDownHandler <- eventListener \e -> case KeyboardEvent.fromEvent e of
    Just keyE | not (repeat keyE) -> do
      fire eventEmitter (KeyEvent { keyCode: Keyboard $ code keyE, keyState: KeyDown })
      modify_ (insert $ Keyboard $ code keyE) keyStateRef
    _ -> pure unit
  addEventListener keydown keyDownHandler false $ w

  keyUpHandler <- eventListener \e -> case KeyboardEvent.fromEvent e of
    Just keyE | not (repeat keyE) -> do
      fire eventEmitter (KeyEvent { keyCode: Keyboard $ code keyE, keyState: KeyUp })
      modify_ (delete $ Keyboard $ code keyE) keyStateRef
    _ -> pure unit
  addEventListener keyup keyUpHandler false $ w

  let
    htmlCanvasElement = unsafeCoerce canvasElement :: HTMLCanvasElement
    cTarget = HTMLCanvasElement.toEventTarget htmlCanvasElement

  mouseDownHandler <- eventListener \e -> case MouseEvent.fromEvent e of
    Just mouseE -> do
      let
        button = case MouseEvent.button mouseE of
          0 -> Just Left
          1 -> Just Center
          2 -> Just Right
          _ -> Nothing
      case button of
        Just b -> do
          fire eventEmitter (KeyEvent { keyCode: Mouse $ b, keyState: KeyDown })
          modify_ (insert $ Mouse $ b) keyStateRef
        Nothing -> pure unit
    _ -> pure unit
  addEventListener mousedown mouseDownHandler false cTarget

  mouseUpHandler <- eventListener \e -> case MouseEvent.fromEvent e of
    Just mouseE -> do
      let
        button = case MouseEvent.button mouseE of
          0 -> Just Left
          1 -> Just Center
          2 -> Just Right
          _ -> Nothing
      case button of
        Just b -> do
          fire eventEmitter (KeyEvent { keyCode: Mouse b, keyState: KeyUp })
          modify_ (delete $ Mouse b) keyStateRef
        Nothing -> pure unit
    _ -> pure unit
  addEventListener mouseup mouseUpHandler false cTarget

  mouseMoveHandler <- eventListener \e -> case MouseEvent.fromEvent e of
    Just mouseE -> do
      { left, top } <- getBoundingClientRect $ HTMLCanvasElement.toHTMLElement htmlCanvasElement
      let
        mouseX = toNumber (MouseEvent.clientX mouseE) - left
        mouseY = toNumber (MouseEvent.clientY mouseE) - top
      fire eventEmitter (MouseMove { mouseX, mouseY })
      write { mouseX, mouseY } mousePositionRef
    _ -> pure unit
  addEventListener mousemove mouseMoveHandler false w

  -- GameStateの初期化
  _ <- liftEffect $ flip runGlappleM internalState do
    gameState <- initGameState
    liftEffect $ write (Just gameState) gameStateRef

  launchAff_ do
    canvasImageSources <- loadImages sprites

    initTime <- liftEffect $ nowTime
    liftEffect $ write (Just initTime) initTimeRef --ゲーム開始時の時刻を保存
    deltaTimeRef <- liftEffect $ new initTime --更新時のdelta Time取得に使うRef

    forever $ do
      procStart <- liftEffect nowTime

      liftEffect $ clearRect offContext2D { x: 0.0, y: 0.0, height, width }

      fire renderEmitter { canvasImageSources, context2D: offContext2D }
      liftEffect $ clearRect context2D { x: 0.0, y: 0.0, height, width }
      liftEffect $ drawImage context2D (canvasElementToImageSource offCanvas) 0.0 0.0

      liftEffect do
        nowT <- nowTime
        prevT <- read deltaTimeRef
        let
          Milliseconds deltaTime = diff nowT prevT
        write nowT deltaTimeRef
        fire eventEmitter (Update { deltaTime: deltaTime / 1000.0 })
      procEnd <- liftEffect nowTime
      let
        Milliseconds dt = diff procEnd procStart

      delay $ Milliseconds $ max 0.0 $ 1000.0 / fps - dt

  pure $ gameId

-- | runGameのOutputHandlerなしバージョン
runGameM_
  :: forall s g i o
   . Ord s
  => Number
  -> CanvasElement
  -> CanvasSpec
  -> Array (s /\ String)
  -> GameSpecM s g i o
  -> Effect (GameId s i)
runGameM_ fps canvasElement { height, width } sprites gameSpecM =
  runGameM fps canvasElement { height, width } sprites gameSpecM \_ -> pure unit