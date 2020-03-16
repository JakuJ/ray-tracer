module Object.Material (
  Material(..),
  MaterialType(..)
) where

import           Common (Color, Point)

-- |Represents the material type
data MaterialType
  = Diffuse
  -- ^Diffuse, opaque material. Does not reflect nor transmit light.
  | Reflection {-# UNPACK #-} !Float
  -- ^Reflective material characterized by a reflection index (0 - not reflective, 1 - perfect mirror).
  | Refraction {-# UNPACK #-} !Float
  -- ^Transparent material characterized by a refraction index.

data Material = Material {
  _materialColor :: Point -> Color,
  -- ^Material color as a normalized RGBA vector.
  -- The alpha channel is used when blending if the material type is 'Refraction'.
  _materialType  :: MaterialType
}
