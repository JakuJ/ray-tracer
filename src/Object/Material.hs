module Object.Material (
  Phong(..),
  Material(..),
  MaterialType(..)
) where

import           Common

-- |Represents the material type.
data MaterialType
  = Diffuse
  -- ^Diffuse, opaque material. Neither reflects nor lets light through.
  | Reflection {-# UNPACK #-} !Double
  -- ^Reflective material characterized by a reflection index (0 - not reflective, 1 - perfect mirror).
  | Refraction {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  -- ^Transparent material characterized by a refraction index and transmittance.

-- |This data type carries colour information used in the Phong reflection model.
data Phong = Phong {
  _color     :: Color,
  _shininess :: {-# UNPACK #-} !Double
}

-- |A data type combining material colour and type properties.
data Material = Material {
  _materialColor :: Phong,
  _materialType  :: MaterialType
}
