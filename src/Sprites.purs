module Sprites where

import Prelude

import Graphics.Glapple.Data.Complex (complex)
import Graphics.Glapple.Data.Picture (Picture, setOrigin, sprite)
import Graphics.Glapple.Data.Sprite (Sprite(..))

sprites :: Array (Sprite Unit)
sprites = [ Source unit "./images/apple.png" ]

apple :: Picture Unit
apple = sprite unit # setOrigin (complex 16.0 16.0)
