module Object.Material (
  -- * Classes
  Material(..),
  Phong(..),
  MaterialType(..),
  -- * Constructors
  plain, uniform, chessboard
) where

import           Common (Color, Point)

import           Linear (V3 (..))

-- |Represents the material type
data MaterialType
  = Diffuse
  -- ^Diffuse, opaque material. Does not reflect nor transmit light.
  | Reflection {-# UNPACK #-} !Double
  -- ^Reflective material characterized by a reflection index (0 - not reflective, 1 - perfect mirror).
  | Refraction {-# UNPACK #-} !Double
  -- ^Transparent material characterized by a refraction index.

data Phong = Phong {
  _ambient  :: Color,
  _diffuse  :: Color,
  _specular :: Color
}

data Material = Material {
  _materialColor :: Point -> Phong,
  -- ^Material color as a normalized RGBA vector.
  -- The alpha channel is used when blending if the material type is 'Refraction'.
  _materialType  :: MaterialType
}

-- Material constructors

plain :: Color -> Phong
plain c = Phong c c c
{-# INLINE plain #-}

uniform :: Phong -> MaterialType -> Material
uniform = Material . const
{-# INLINE uniform #-}

chessboard :: Color -> Color -> MaterialType -> Material
chessboard c1 c2 = Material (plain . pattern)
  where
    pattern (V3 x _ z) = if check x == check z then c1 else c2
    check = even . (floor :: Double -> Int)
