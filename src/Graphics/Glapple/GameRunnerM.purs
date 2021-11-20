-- | runGameMでゲームを実行します．
module Graphics.Glapple.GameRunnerM (runChildGameM, runChildGameM_, runGameM, runGameM_) where

import Prelude

import Control.Monad.Reader (ask)
import Control.Monad.Rec.Class (forever)
import Data.Int (toNumber)
import Data.Map (fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Data.Time (diff)
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Exception (catchException)
import Effect.Now (nowTime)
import Effect.Ref (new, read, write)
import Graphics.Glapple.GlappleM (GlappleM, InternalState, runGlappleM)
import Graphics.Canvas (CanvasElement, CanvasImageSource, Context2D, canvasElementToImageSource, clearRect, drawImage, getContext2D, setCanvasHeight, setCanvasWidth)
import Graphics.Glapple.Data.Emitter (fire, newEmitter, register)
import Graphics.Glapple.Data.Event (Event(..), KeyState(..))
import Graphics.Glapple.Data.GameId (GameId(..))
import Graphics.Glapple.Data.GameSpecM (CanvasSpec, GameSpecM(..))
import Graphics.Glapple.Data.Picture (Picture, drawPicture, empty, tryLoadImageAff)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.KeyboardEvent (key, repeat)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

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
  let
    f x = do --エラー処理
      logShow x
      pure empty
  pic <- liftEffect $ catchException f $ runGlappleM render internalState
  drawPicture context2D canvasImageSources pic

makeHandlerEffect
  :: forall s g i o a
   . InternalState s g i o
  -> (a -> GlappleM s g i o Unit)
  -> (a -> Effect Unit)
makeHandlerEffect internalState eventHandler = \e -> catchException logShow
  $ runGlappleM (eventHandler e) internalState

-- | 現在のゲームの中で，新しく子ゲームを作る
runChildGameM
  :: forall s g i o childG childI childO
   . GameSpecM s childG childI childO
  -> (childO -> Effect Unit)
  -> GlappleM s g i o (GameId s childI childO)
runChildGameM (GameSpecM { initGameState, render, eventHandler, inputHandler }) outputHandler = do
  { eventEmitter, initTimeRef } <- ask
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
      }

  let
    inputHandler_ = makeHandlerEffect childInternalState inputHandler
    renderHandler_ = makeRenderHandler childInternalState render
    eventHandler_ = makeHandlerEffect childInternalState eventHandler

  inputId <- register inputEmitter inputHandler_
  outputId <- register outputEmitter outputHandler
  renderId <- register renderEmitter renderHandler_
  eventId <- register eventEmitter eventHandler_

  let
    internalRegistrationIds = { inputId, outputId, eventId, renderId }
    gameId = GameId { inputEmitter, renderEmitter, internalRegistrationIds }

  liftEffect $ write (Just internalRegistrationIds) internalRegistrationIdsRef

  liftEffect $ flip runGlappleM childInternalState do
    gameState <- initGameState
    liftEffect $ write (Just gameState) gameStateRef

  pure gameId

runChildGameM_
  :: forall s g i o childG childI childO
   . GameSpecM s childG childI childO
  -> GlappleM s g i o (GameId s childI childO)
runChildGameM_ gameSpecM = runChildGameM gameSpecM \_ -> pure unit

--------------
-- Run Game --
-- --------------

loadImages :: forall s. Ord s => Array (s /\ String) -> Aff (s -> Maybe CanvasImageSource)
loadImages sprites = do
  tmp <- map fromFoldable
    $ for sprites (\(sprite /\ src) -> (sprite /\ _) <$> tryLoadImageAff src)
  pure \s -> lookup s tmp

-- | トップレベルでゲームを作る
runGameM
  :: forall s g i o
   . Ord s
  => Int
  -> CanvasElement
  -> CanvasSpec
  -> Array (s /\ String)
  -> GameSpecM s g i o
  -> (o -> Effect Unit)
  -> Effect (GameId s i o)

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
    gameId = GameId { inputEmitter, renderEmitter, internalRegistrationIds }

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
  w <- window

  -- Web EventでEmitterを発火させる
  let
    makeHandler x = eventListener \e -> case KeyboardEvent.fromEvent e of
      Just keyE | not (repeat keyE) -> fire eventEmitter (KeyEvent (key keyE) x)
      _ -> pure unit
  keyDownHandler <- makeHandler KeyDown
  keyUpHandler <- makeHandler KeyUp
  addEventListener (EventType "keydown") keyDownHandler false $ Window.toEventTarget w
  addEventListener (EventType "keyup") keyUpHandler false $ Window.toEventTarget w

  -- GameStateの初期化
  liftEffect $ flip runGlappleM internalState do
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

      delay $ Milliseconds $ max 0.0 $ 1000.0 / toNumber fps - dt

  pure $ gameId

-- | runGameのOutputHandlerなしバージョン
runGameM_
  :: forall s g i o
   . Ord s
  => Int
  -> CanvasElement
  -> CanvasSpec
  -> Array (s /\ String)
  -> GameSpecM s g i o
  -> Effect (GameId s i o)
runGameM_ fps canvasElement { height, width } sprites gameSpecM =
  runGameM fps canvasElement { height, width } sprites gameSpecM \_ -> pure unit