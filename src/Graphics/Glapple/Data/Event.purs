module Graphics.Glapple.Data.Event where

import Prelude

import Data.Time.Duration (Milliseconds)

data KeyState = KeyDown | KeyUp

derive instance Eq KeyState

data Event = KeyEvent String KeyState | Update Milliseconds

derive instance Eq input => Eq Event
