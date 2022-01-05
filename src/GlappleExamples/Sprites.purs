module GlappleExamples.Sprites
  ( SpriteType(..)
  , sprites
  ) where

import Prelude

import Data.Hashable (class Hashable)
import Graphics.Glapple.Data.Sprite (Sprite(..))

data SpriteType = Apple

derive instance Eq SpriteType
instance Hashable SpriteType where
  hash Apple = 0

sprites :: Array (Sprite SpriteType)
sprites = [ Source Apple "./images/apple.png" ]
