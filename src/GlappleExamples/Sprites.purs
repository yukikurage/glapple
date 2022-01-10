module GlappleExamples.Sprites
  ( SpriteType(..)
  , sprites
  ) where

import Prelude

import Graphics.Glapple.Data.Sprite (Sprite(..))

data SpriteType = Apple

derive instance Eq SpriteType
derive instance Ord SpriteType

sprites :: Array (Sprite SpriteType)
sprites = [ Source Apple "./images/apple.png" ]
