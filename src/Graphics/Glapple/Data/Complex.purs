-- | 複素数を表す型．画面上の位置は基本的にこれで表される．

module Graphics.Glapple.Data.Complex where

import Prelude

import Math (atan2)

newtype Complex a = Complex { real :: a, imaginary :: a }

derive newtype instance Eq a => Eq (Complex a)
derive newtype instance Show a => Show (Complex a)

instance Ring a => Semiring (Complex a) where
  add (Complex { real: r0, imaginary: i0 }) (Complex { real: r1, imaginary: i1 }) =
    Complex { real: r0 + r1, imaginary: i0 + i1 }
  zero = Complex { real: zero, imaginary: zero }
  mul (Complex { real: r0, imaginary: i0 }) (Complex { real: r1, imaginary: i1 }) =
    Complex { real: r0 * r1 - i0 * i1, imaginary: r0 * i1 + i0 * r1 }
  one = Complex { real: one, imaginary: zero }

instance Ring a => Ring (Complex a) where
  sub (Complex { real: r0, imaginary: i0 }) (Complex { real: r1, imaginary: i1 }) =
    Complex { real: r0 - r1, imaginary: i0 - i1 }

instance CommutativeRing a => CommutativeRing (Complex a)

instance DivisionRing a => DivisionRing (Complex a) where
  recip (Complex { real: x, imaginary: y }) =
    Complex { real: x * recip (x * x - y * y), imaginary: -y * recip (x * x - y * y) }

instance EuclideanRing (Complex Number) where
  degree _ = 1
  div x y = x * recip y
  mod _ _ = zero

i :: forall a. Semiring a => Complex a
i = Complex { real: zero, imaginary: one }

magnitude :: forall a. Semiring a => Complex a -> a
magnitude (Complex { real, imaginary }) = real * real + imaginary * imaginary

arg :: Complex Number -> Number
arg (Complex { real, imaginary }) = atan2 real imaginary

scalarLeftMul :: forall a. Semiring a => a -> Complex a -> Complex a
scalarLeftMul a (Complex { real, imaginary }) = Complex { real: a * real, imaginary: a * imaginary }

scalarRightMul :: forall t140. Semiring t140 => t140 -> Complex t140 -> Complex t140
scalarRightMul a (Complex { real, imaginary }) = Complex { real: real * a, imaginary: imaginary * a }

infix 7 scalarLeftMul as *~
infix 7 scalarRightMul as ~*

-- | 複素数zに対し z * x の z * を行列で表現する
toTransformMatrixMul
  :: forall a
   . Ring a
  => Complex a
  -> { m11 :: a
     , m12 :: a
     , m21 :: a
     , m22 :: a
     , m31 :: a
     , m32 :: a
     }
toTransformMatrixMul (Complex { real, imaginary }) =
  { m11: real
  , m12: real
  , m21: -imaginary
  , m22: imaginary
  , m31: zero
  , m32: zero
  }

-- | 複素数zに対し z + x の z + を行列で表現する
toTransformMatrixAdd
  :: forall a
   . Ring a
  => Complex a
  -> { m11 :: a
     , m12 :: a
     , m21 :: a
     , m22 :: a
     , m31 :: a
     , m32 :: a
     }
toTransformMatrixAdd (Complex { real, imaginary }) =
  { m11: one
  , m12: zero
  , m21: zero
  , m22: one
  , m31: real
  , m32: imaginary
  }