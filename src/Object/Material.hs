module Object.Material (
  -- * Data types
  Material(..), MaterialType(..),
  -- * Blending functions
  alphaBlend,
  -- * Miscellanea
  background
) where

import           Common       (Color, Point)

import           Control.Lens ((^.))
import           Linear

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

background :: Color
background = V4 0 0 0 1

-- |Blend two RGBA 'Color' vectors. If the second one is 'Nothing', treat it as black.
alphaBlend :: Color -> Color -> Color
alphaBlend c1 c2 = V4 r g b out_a
  where
    a1 = c1 ^. _w
    a2 = (c2 ^. _w) * (1 - a1)
    out_a = a1 + a2
    (V3 r g b) = ((c1 ^. _xyz) ^* a1 ^+^ (c2 ^. _xyz) ^* a2) ^/ out_a
