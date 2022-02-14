module Components.Root where

import Prelude

import Components.ThrownApple (thrownApple)
import Data.Tuple.Nested ((/\))
import Graphics.Glapple.Data.Component (Component)
import Graphics.Glapple.Data.KeyEvent (KeyCode(..), KeyEvent(..), MouseButton(..))
import Graphics.Glapple.Data.Picture (line)
import Graphics.Glapple.Hooks.UseKeyEvent (useKeyEvent)
import Graphics.Glapple.Hooks.UseRenderer (useRenderer)
import Graphics.Glapple.Hooks.UseRunner (useChildRunner)
import Graphics.Glapple.Hooks.UseTransform (useGlobalTranslate, useTranslateNow)
import Graphics.Glapple.UseMouseState (useMouseState)

startPoint ::
  { x :: Number
  , y :: Number
  }
startPoint = { x: 10.0, y: 10.0 }

-- | 球を投げる場所
root :: {} -> Component Unit Unit
root _ = do
  useTranslateNow startPoint --原点に位置を設定

  getMousePos <- useMouseState
  getGlobalTranslate <- useGlobalTranslate

  useRenderer 0.0 do
    { x: msX, y: msY } <- getMousePos --マウスの位置を取得
    { x: glX, y: glY } <- getGlobalTranslate --グローバル座標を取得
    pure $ line [ 0.0 /\ 0.0, (msX - glX) /\ (msY - glY) ]

  runApple /\ _ <- useChildRunner thrownApple

  useKeyEvent case _ of
    KeyDown (Mouse Left) -> do
      { x: msX, y: msY } <- getMousePos --マウスの位置を取得
      { x: glX, y: glY } <- getGlobalTranslate --グローバル座標を取得
      runApple { x: msX - glX, y: msY - glY }
    _ -> pure unit
