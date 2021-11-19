module Graphics.Glapple.Data.Picture where

import Prelude

import Color (Color, cssStringRGBA)
import Data.Array (uncons)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, error, makeAff)
import Effect.Class (liftEffect)
import Graphics.Canvas (CanvasGradient, CanvasImageSource, CanvasPattern, Composite(..), Context2D, PatternRepeat, TextAlign, TextBaseline, Transform, addColorStop, beginPath, closePath, createLinearGradient, createPattern, createRadialGradient, drawImage, fill, lineTo, moveTo, restore, save, setGlobalCompositeOperation, setGradientFillStyle, setLineWidth, setPatternFillStyle, setTextAlign, setTextBaseline, stroke, tryLoadImage)
import Graphics.Canvas as C

newtype Picture sprite = Picture (Context2D -> (sprite -> Maybe CanvasImageSource) -> Aff Unit)

instance Semigroup (Picture sprite) where
  append = composite SourceOver

instance Monoid (Picture sprite) where
  mempty = empty

drawPicture
  :: forall sprite
   . Context2D
  -> (sprite -> Maybe CanvasImageSource)
  -> Picture sprite
  -> Aff Unit
drawPicture ctx canvasImageSources (Picture f) = f ctx canvasImageSources

-- | 画像の読み込み
tryLoadImageAff :: String -> Aff CanvasImageSource
tryLoadImageAff str = makeAff \thrower -> do
  tryLoadImage str $ case _ of
    Just x -> thrower $ Right x
    Nothing -> thrower $ Left $ error $ "Image LoadingError: " <> str
  pure mempty

data DrawStyle sprite
  = LinearGradient
      { x0 :: Number
      , y0 :: Number
      , x1 :: Number
      , y1 :: Number
      , colorStops :: Array (Number /\ Color)
      }
  | RadialGradient
      { x0 :: Number
      , y0 :: Number
      , r0 :: Number
      , x1 :: Number
      , y1 :: Number
      , r1 :: Number
      , colorStops :: Array (Number /\ Color)
      }
  | Pattern { sprite :: sprite, repeat :: PatternRepeat }
  | MonoColor Color

data Shape = Fill | Stroke

runShape :: Context2D -> Shape -> Effect Unit
runShape ctx = case _ of
  Fill -> do
    fill ctx
  Stroke -> do
    stroke ctx

foreign import setGradientStrokeStyle :: Context2D -> CanvasGradient -> Effect Unit
foreign import setPatternStrokeStyle :: Context2D -> CanvasPattern -> Effect Unit

setDrawStyle :: forall s. Context2D -> (s -> Maybe CanvasImageSource) -> DrawStyle s -> Effect Unit
setDrawStyle ctx canvasImageSources = case _ of
  LinearGradient { x0, y0, x1, y1, colorStops } -> do
    gradient <- createLinearGradient ctx { x0, y0, x1, y1 }
    for_ colorStops \(x /\ col) -> addColorStop gradient x $ cssStringRGBA col
    setGradientFillStyle ctx gradient
    setGradientStrokeStyle ctx gradient
  RadialGradient { x0, y0, x1, y1, r0, r1, colorStops } -> do
    gradient <- createRadialGradient ctx { x0, y0, x1, y1, r0, r1 }
    for_ colorStops \(x /\ col) -> addColorStop gradient x $ cssStringRGBA col
    setGradientFillStyle ctx gradient
    setGradientStrokeStyle ctx gradient
  Pattern { sprite: spr, repeat } -> case canvasImageSources spr of
    Nothing -> pure unit
    Just canvasImageSource -> do
      pattern <- createPattern ctx canvasImageSource repeat
      setPatternFillStyle ctx pattern
      setPatternStrokeStyle ctx pattern
  MonoColor c -> do
    C.setFillStyle ctx $ cssStringRGBA c
    C.setStrokeStyle ctx $ cssStringRGBA c

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
  C.setFont ctx $ show fontStyle
    <> " "
    <> show fontWeight
    <> " "
    <> show fontSize
    <> "px/"
    <> show fontHeight
    <> "px "
    <> show fontFamily

------------------------
-- Picture Operations --
------------------------

composite :: forall sprite. Composite -> Picture sprite -> Picture sprite -> Picture sprite
composite comp pic1 pic2 =
  Picture \ctx canvasImageSources -> do
    drawPicture ctx canvasImageSources pic1
    liftEffect $ save ctx
    liftEffect $ setGlobalCompositeOperation ctx comp
    liftEffect $ beginPath ctx --現在の描画の状態をリセット
    drawPicture ctx canvasImageSources pic2
    liftEffect $ restore ctx

sourceOverComposite :: forall s. Picture s -> Picture s -> Picture s
sourceOverComposite = composite SourceOver

destinationOverComposite :: forall s. Picture s -> Picture s -> Picture s
destinationOverComposite = composite DestinationOver

multiplyComposite :: forall s. Picture s -> Picture s -> Picture s
multiplyComposite = composite Multiply

addComposite :: forall s. Picture s -> Picture s -> Picture s
addComposite = composite Lighter

infixl 5 sourceOverComposite as <-^
infixl 5 destinationOverComposite as <-.
infixl 5 multiplyComposite as <-*
infixl 5 addComposite as <-+

translate :: forall s. Number -> Number -> Picture s -> Picture s
translate x y pic = Picture \ctx canvasImageSources -> do
  liftEffect $ save ctx
  liftEffect $ C.translate ctx { translateX: x, translateY: y }
  drawPicture ctx canvasImageSources pic
  liftEffect $ restore ctx

scale :: forall s. Number -> Number -> Picture s -> Picture s
scale sx sy pic = Picture \ctx canvasImageSources -> do
  liftEffect $ save ctx
  liftEffect $ C.scale ctx { scaleX: sx, scaleY: sy }
  drawPicture ctx canvasImageSources pic
  liftEffect $ restore ctx

-- | 右回転
rotate :: forall s. Number -> Picture s -> Picture s
rotate r pic = Picture \ctx canvasImageSources -> do
  liftEffect $ save ctx
  liftEffect $ C.rotate ctx r
  drawPicture ctx canvasImageSources pic
  liftEffect $ restore ctx

transform
  :: forall s
   . Transform
  -> Picture s
  -> Picture s
transform trans pic = Picture \ctx canvasImageSources -> do
  liftEffect $ save ctx
  liftEffect $ C.transform ctx trans
  drawPicture ctx canvasImageSources pic
  liftEffect $ restore ctx

------------
-- Shapes --
------------

-- | Pictureに何らかのプロパティをつける
operate :: forall s. (Context2D -> Effect Unit) -> Picture s -> Picture s
operate f p = Picture \ctx img -> do
  liftEffect $ save ctx
  liftEffect $ f ctx
  drawPicture ctx img p
  liftEffect $ restore ctx

empty :: forall sprite. Picture sprite
empty = Picture \_ _ -> pure unit

sprite :: forall sprite. sprite -> Picture sprite
sprite spr = Picture \ctx canvasImageSources -> case canvasImageSources spr of
  Nothing -> pure unit
  Just x -> do
    liftEffect $ drawImage ctx x 0.0 0.0

image :: forall s. String -> Picture s
image str = Picture \ctx _ -> do
  imgSource <- tryLoadImageAff str
  liftEffect $ drawImage ctx imgSource 0.0 0.0

-- | 色をつけます
draw :: forall s. DrawStyle s -> Picture s -> Picture s
draw drawStyle shape = Picture \ctx img -> do
  liftEffect $ setDrawStyle ctx img drawStyle
  drawPicture ctx img shape

color :: forall s. Color -> Picture s -> Picture s
color c s = draw (MonoColor c) s

textAlign :: forall s. TextAlign -> Picture s -> Picture s
textAlign a = operate (flip setTextAlign a)

font :: forall s. Font -> Picture s -> Picture s
font f = operate (flip setFont f)

textBaseLine :: forall s. TextBaseline -> Picture s -> Picture s
textBaseLine b = operate (flip setTextBaseline b)

lineWidth :: forall s. Number -> Picture s -> Picture s
lineWidth w = operate $ flip setLineWidth w

text
  :: forall sprite
   . Shape
  -> String
  -> Picture sprite
text style str = Picture \ctx _ -> liftEffect do
  save ctx
  case style of
    Fill -> do
      C.fillText ctx str 0.0 0.0
    Stroke -> do
      C.strokeText ctx str 0.0 0.0
  restore ctx

polygon
  :: forall sprite
   . Shape
  -> Array (Number /\ Number)
  -> Picture sprite
polygon style path = Picture \ctx _ -> liftEffect do
  save ctx
  case uncons path of
    Just { head: (hx /\ hy), tail } -> do
      beginPath ctx
      moveTo ctx hx hy
      for_ tail \(x /\ y) -> lineTo ctx x y
      closePath ctx
    Nothing -> pure unit
  runShape ctx style
  restore ctx

line
  :: forall sprite
   . Array (Number /\ Number)
  -> Picture sprite
line path = Picture \ctx _ -> liftEffect do
  save ctx
  case uncons path of
    Just { head: (hx /\ hy), tail } -> do
      beginPath ctx
      moveTo ctx hx hy
      for_ tail \(x /\ y) -> lineTo ctx x y
      closePath ctx
    Nothing -> pure unit
  runShape ctx $ Stroke
  restore ctx

rect
  :: forall s
   . Shape
  -> Number
  -> Number
  -> Picture s
rect style height width = Picture \ctx _ -> liftEffect do
  save ctx
  C.rect ctx { x: 0.0, y: 0.0, height, width }
  runShape ctx style
  restore ctx

arc :: forall s. { start :: Number, end :: Number, radius :: Number } -> Picture s
arc { start, end, radius } = Picture \ctx _ -> liftEffect $ do
  save ctx
  C.arc ctx { x: 0.0, y: 0.0, start, end, radius }
  runShape ctx $ Stroke
  restore ctx

fan :: forall s. Shape -> { start :: Number, end :: Number, radius :: Number } -> Picture s
fan style { radius, start, end } = Picture \ctx _ -> liftEffect do
  save ctx
  moveTo ctx 0.0 0.0
  C.arc ctx { x: 0.0, y: 0.0, start, end, radius }
  closePath ctx
  runShape ctx style