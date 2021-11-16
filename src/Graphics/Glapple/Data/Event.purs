module Graphic.Glapple.Data.Event where

data Event input = KeyEvent String KeyState | Update Number | Input input

data KeyState = KeyDown | KeyUp