module Object.Material (
  -- * Classes
  Material(..),
  Phong(..),
  MaterialType(..)
) where

import           Common

-- |Represents the material type
data MaterialType
  = Diffuse
  -- ^Diffuse, opaque material. Does not reflect nor transmit light.
  | Reflection {-# UNPACK #-} !Double
  -- ^Reflective material characterized by a reflection index (0 - not reflective, 1 - perfect mirror).
  | Refraction {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  -- ^Transparent material characterized by a refraction index and transmittance.
    deriving (Show)

data Phong = Phong {
  _color     :: Color,
  _shininess :: {-# UNPACK #-} !Double
}

data Material = Material {
  _materialColor :: Phong,
  _materialType  :: MaterialType
}

instance Show Material where
  show (Material _ mt) = "Material with: " ++ show mt
