-- | runGameMでゲームを実行します．
module Graphics.Glapple.GameRunnerM (runGameM, runGameM_, runChildGameM, runChildGameM_) where

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
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now (nowTime)
import Effect.Ref (new, read, write)
import Graphics.Glapple.Data.Event (Event(..), KeyState(..))
import Graphic.Glapple.GlappleM (GlappleM, InternalState, runGlappleM)
import Graphics.Canvas (CanvasElement, CanvasImageSource, canvasElementToImageSource, clearRect, drawImage, getContext2D, setCanvasHeight, setCanvasWidth)
import Graphics.Glapple.Data.Emitter (RegistrationId, fire, newEmitter, register)
import Graphics.Glapple.Data.GameId (GameId(..))
import Graphics.Glapple.Data.GameSpecM (CanvasSpec, GameSpecM(..))
import Graphics.Glapple.Data.Picture (Picture, drawPicture, tryLoadImageAff)
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

-- | "親の"GlappleM内で子のGameSpecMから子のInternalStateを作成します
makeInternalState
  :: forall s g o childG childO
   . GlappleM s g o (InternalState s childG childO)
makeInternalState = do
  { eventEmitter, initTimeRef, canvasImageSources, context2D } <- ask
  outputEmitter <- newEmitter
  gameStateRef <- liftEffect $ new Nothing
  pure { eventEmitter, outputEmitter, initTimeRef, gameStateRef, canvasImageSources, context2D }

-- | "子の"GlappleM内でrenderを作成
makeRenderHandler :: forall s g o. GlappleM s g o (Picture s) -> GlappleM s g o (Unit -> Aff Unit)
makeRenderHandler render = do
  internalState@{ canvasImageSources, context2D } <- ask
  pure \_ -> do --ここ以下がレンダリング毎に実行される
    pic <- liftEffect $ runGlappleM render internalState
    drawPicture context2D canvasImageSources pic

makeHandlerEffect :: forall s g o a. (a -> GlappleM s g o Unit) -> GlappleM s g o (a -> Effect Unit)
makeHandlerEffect eventHandler = do
  internalState <- ask
  pure \e -> do --ここ以下がイベント発火ごとに実行される
    runGlappleM (eventHandler e) internalState

-- | "子の"GlappleM内でGameIdを作成
makeGameId :: forall m m'. Bind m => MonadEffect m => m (GameId m')
makeGameId = do
  inputEmitter <- newEmitter
  renderEmitter <- newEmitter
  pure $ GameId { inputEmitter, renderEmitter }

-- | "子の"GlappleM内でrenderEmitterにrenderを登録
registerRenderEmitter
  :: forall i m
   . MonadEffect m
  => GameId i
  -> (Unit -> Aff Unit)
  -> m (RegistrationId Aff Unit)
registerRenderEmitter (GameId { renderEmitter }) render = register renderEmitter render

-- | "子の"GlappleM内でinputEmitterにinputHandlerを登録
registerInputEmitter
  :: forall i m
   . MonadEffect m
  => GameId i
  -> (i -> Effect Unit)
  -> m (RegistrationId Effect i)
registerInputEmitter (GameId { inputEmitter }) handler = register inputEmitter handler

-- | "子の"GlappleM内でEventEmitterにeventHandlerを登録
registerEventEmitter
  :: forall s g o
   . (Event -> Effect Unit)
  -> GlappleM s g o (RegistrationId Effect Event)
registerEventEmitter handler = do
  { eventEmitter } <- ask
  liftEffect $ register eventEmitter handler

-- | 現在のゲームの中で，新しく子ゲームを作る
runChildGameM
  :: forall s g o childG childI childO
   . GameSpecM s childG childI childO
  -> (childO -> Effect Unit)
  -> GlappleM s g o (GameId childI)
runChildGameM (GameSpecM { initGameState, render, eventHandler, inputHandler }) outputHandler = do
  childInternalState@{ outputEmitter, gameStateRef } <- makeInternalState
  let
    mainProc = do
      gameState <- initGameState
      liftEffect $ write (Just gameState) gameStateRef

      renderHandler <- makeRenderHandler render
      eventHandlerEffect <- makeHandlerEffect eventHandler
      inputHandlerEffect <- makeHandlerEffect inputHandler
      gameId <- makeGameId
      _ <- registerRenderEmitter gameId renderHandler
      _ <- registerInputEmitter gameId inputHandlerEffect
      _ <- registerEventEmitter eventHandlerEffect --後から接続を切りたいならここらへんの値を保存しておく必要アリ
      _ <- register outputEmitter outputHandler
      pure gameId
  liftEffect $ runGlappleM mainProc childInternalState

runChildGameM_
  :: forall s g o childG childI childO
   . GameSpecM s childG childI childO
  -> GlappleM s g o (GameId childI)
runChildGameM_ gameSpecM = runChildGameM gameSpecM \_ -> pure unit

--------------
-- Run Game --
--------------

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
  -> Effect (GameId i)

runGameM
  fps
  canvasElement
  { height, width }
  sprites
  (GameSpecM { initGameState, render, eventHandler, inputHandler })
  outputHandler = do

  -- 様々なRefを定義
  gameStateRef <- new Nothing
  deltaTimeRef <- new =<< nowTime
  initTimeRef <- new Nothing

  -- Emitterを作成
  inputEmitter <- newEmitter
  outputEmitter <- newEmitter
  renderEmitter <- newEmitter
  eventEmitter <- newEmitter
  let
    gameId = GameId { inputEmitter, renderEmitter }

  -- キャンバス系
  offCanvas <- createCanvasElement --裏画面
  offContext2D <- getContext2D offCanvas --裏画面のcontext2D
  context2D <- getContext2D canvasElement
  setCanvasHeight offCanvas height
  setCanvasWidth offCanvas width
  setCanvasHeight canvasElement height
  setCanvasWidth canvasElement width

  -- Windowを
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

  launchAff_ do
    canvasImageSources <- loadImages sprites
    let
      internal = { eventEmitter, outputEmitter, initTimeRef, gameStateRef, canvasImageSources, context2D: offContext2D }
    -- GlappleM内での処理
    _ <- liftEffect $ flip runGlappleM internal do
      initGame <- initGameState
      liftEffect $ write (Just initGame) gameStateRef

      renderHandler <- makeRenderHandler render
      eventHandlerEffect <- makeHandlerEffect eventHandler
      inputHandlerEffect <- makeHandlerEffect inputHandler
      -- もろもろのエミッターに登録
      _ <- registerRenderEmitter gameId renderHandler
      _ <- registerInputEmitter gameId inputHandlerEffect
      _ <- registerEventEmitter eventHandlerEffect
      _ <- register outputEmitter outputHandler --後から接続を切りたいならここらへんの値を保存しておく必要アリ
      pure unit

    nowT <- liftEffect $ nowTime
    liftEffect $ write (Just nowT) initTimeRef

    forever $ do
      procStart <- liftEffect nowTime

      liftEffect $ clearRect offContext2D { x: 0.0, y: 0.0, height, width }
      fire renderEmitter unit
      liftEffect $ clearRect context2D { x: 0.0, y: 0.0, height, width }
      liftEffect $ drawImage context2D (canvasElementToImageSource offCanvas) 0.0 0.0

      liftEffect do
        now <- nowTime
        prevT <- read deltaTimeRef
        let
          d = diff now prevT
        write now deltaTimeRef
        fire eventEmitter (Update d)
      procEnd <- liftEffect nowTime
      let
        Milliseconds dt = diff procEnd procStart

      delay $ Milliseconds $ max 0.0 $ 1000.0 / toNumber fps - dt

  pure $ gameId

-- | runGameのOutputHandlerなしバージョン
runGameM_
  :: forall sprite gameState input output
   . Ord sprite
  => Int
  -> CanvasElement
  -> CanvasSpec
  -> Array (sprite /\ String)
  -> GameSpecM sprite gameState input output
  -> Effect (GameId input)
runGameM_ fps canvasElement { height, width } sprites gameSpecM =
  runGameM fps canvasElement { height, width } sprites gameSpecM \_ -> pure unit