module Graphic.Glapple.Data.Event where

import Prelude

import Data.Time.Duration (Milliseconds)

data KeyState = KeyDown | KeyUp

derive instance Eq KeyState

data Event input = KeyEvent String KeyState | Update Milliseconds | Input input

derive instance Eq input => Eq (Event input)
