module TestComponents.Sprites where

import Prelude

import Color (rgb')
import Data.Array (fold, range)
import Data.Int (toNumber)
import Graphics.Glapple.Data.Picture (Picture, arc, color, lineWidth, translate)
import Graphics.Glapple.Data.SpriteData (SpriteData(..))
import Math (pi)

data Sprite = Apple | ArcTest

derive instance Eq Sprite
derive instance Ord Sprite

sprites :: Array (SpriteData Sprite)
sprites = [ FromImage Apple "/images/apple.png", FromPicture ArcTest arcTest ]

arcTest :: forall s. Picture s
arcTest = lineWidth 4.0 $ color (rgb' 1.0 0.3 1.0) $ fold do
  i <- range (-4) 4
  j <- range (-8) 8
  let
    i' = toNumber i
    j' = toNumber j
    start = i' * pi / 2.0
    angle = j' * pi / 2.0
  pure $ translate ((j' + 8.0) * 18.0 + 9.0) ((i' + 4.0) * 18.0 + 9.0) $ arc { start, angle, radius: 6.0 }