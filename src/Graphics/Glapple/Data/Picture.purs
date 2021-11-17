module Graphics.Glapple.Data.Picture where

import Prelude

import Color (Color, cssStringRGBA)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas (CanvasGradient, CanvasImageSource, CanvasPattern, Composite(..), Context2D, TextAlign, TextBaseline, Transform, TranslateTransform, drawImage, setGlobalCompositeOperation, setGradientFillStyle, setPatternFillStyle, setTextAlign, setTextBaseline, setTransform)
import Graphics.Canvas as C

newtype Picture sprite = Picture (Context2D -> (sprite -> Maybe CanvasImageSource) -> Effect Unit)

instance Semigroup (Picture sprite) where
  append = sourceOverComposite

instance Monoid (Picture sprite) where
  mempty = empty

drawPicture
  :: forall sprite
   . Context2D
  -> (sprite -> Maybe CanvasImageSource)
  -> Picture sprite
  -> Effect Unit
drawPicture ctx canvasImagesSources (Picture f) = f ctx canvasImagesSources

data FillStyle = FillGradient CanvasGradient | FillPattern CanvasPattern | FillColor Color
data DrawStyle = Fill FillStyle | Stroke Color

setFillStyle :: Context2D -> FillStyle -> Effect Unit
setFillStyle ctx = case _ of
  FillGradient g -> setGradientFillStyle ctx g
  FillPattern p -> setPatternFillStyle ctx p
  FillColor c -> C.setFillStyle ctx $ cssStringRGBA c

setStrokeStyle :: Context2D -> Color -> Effect Unit
setStrokeStyle ctx c = C.setStrokeStyle ctx $ cssStringRGBA c

data FontStyle = FontStyleNormal | Oblique | Italic

instance Show FontStyle where
  show = case _ of
    FontStyleNormal -> "normal"
    Oblique -> "oblique"
    Italic -> "italic"

data FontWeight = FontWeightNormal | Bold | FontWeight Int

instance Show FontWeight where
  show = case _ of
    FontWeightNormal -> "normal"
    Bold -> "bold"
    FontWeight i -> show i

data FontFamily = Serif | SansSerif | Cursive | Fantasy | Monospace

instance Show FontFamily where
  show = case _ of
    Serif -> "serif"
    SansSerif -> "bold"
    Cursive -> "cursive"
    Fantasy -> "fantasy"
    Monospace -> "monospace"

newtype Font = Font
  { fontStyle :: FontStyle
  , fontWeight :: FontWeight
  , fontSize :: Int
  , fontHeight :: Int
  , fontFamily :: FontFamily
  }

setFont :: Context2D -> Font -> Effect Unit
setFont ctx (Font { fontStyle, fontWeight, fontSize, fontHeight, fontFamily }) = do
  C.setFont ctx $ show fontStyle <> " " <> show fontWeight <> " " <> show fontSize <> "px/" <> show fontHeight <> "px " <> show fontFamily

---------------
-- Operation --
---------------

composite :: forall sprite. Composite -> Picture sprite -> Picture sprite -> Picture sprite
composite comp pic1 pic2 =
  Picture \ctx canvasImageSources -> do
    setGlobalCompositeOperation ctx comp
    drawPicture ctx canvasImageSources pic1
    drawPicture ctx canvasImageSources pic2
    setGlobalCompositeOperation ctx SourceOver

sourceOverComposite :: forall sprite. Picture sprite -> Picture sprite -> Picture sprite
sourceOverComposite = composite SourceOver

resetTransForm :: Context2D -> Effect Unit
resetTransForm ctx = setTransform ctx { m11: 1.0, m12: 0.0, m21: 0.0, m22: 1.0, m31: 0.0, m32: 0.0 }

translate :: forall sprite. TranslateTransform -> Picture sprite -> Picture sprite
translate trans pic = Picture \ctx canvasImageSources -> do
  C.translate ctx trans
  drawPicture ctx canvasImageSources pic
  resetTransForm ctx

--------------
-- Pictures --
--------------

empty :: forall sprite. Picture sprite
empty = Picture \_ _ -> pure unit

sprite :: forall sprite. sprite -> Picture sprite
sprite spr = Picture \ctx canvasImageSources -> case canvasImageSources spr of
  Nothing -> pure unit
  Just x -> drawImage ctx x 0.0 0.0

fillText :: forall sprite. DrawStyle -> TextAlign -> TextBaseline -> Font -> String -> Picture sprite
fillText style align baseline font str = Picture \ctx _ -> do
  setTextAlign ctx align
  setTextBaseline ctx baseline
  setFont ctx font
  case style of
    Fill s -> do
      setFillStyle ctx s
      C.fillText ctx str 0.0 0.0
    Stroke c -> do
      setStrokeStyle ctx c
      C.strokeText ctx str 0.0 0.0