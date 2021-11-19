module TestComponents.Sprites where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))

data Sprite = Apple

derive instance Eq Sprite
derive instance Ord Sprite

sprites :: Array (Sprite /\ String)
sprites = [ Apple /\ "/images/apple.png" ]