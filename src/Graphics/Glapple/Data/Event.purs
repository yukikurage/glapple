module Graphics.Glapple.Data.Event where

import Prelude

data KeyState = KeyDown | KeyUp

derive instance Eq KeyState

-- | deltaTimeは前回からの経過時間(s)
data Event = KeyEvent String KeyState | Update { deltaTime :: Number }

derive instance Eq input => Eq Event
