{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Shapes where

import           Control.Lens
import           Linear
import           Types

data Ray = Ray {_pos :: Point, _dir :: Vector}

class Traceable a where
    colorAt :: a -> Ray -> Point -> Color
    intPoint :: Ray -> a -> Maybe Point
    intColor :: Ray -> a -> Maybe Color
    intColor r t = colorAt t r <$> intPoint r t
    {-# MINIMAL colorAt, intPoint #-}

data Material = Material {
    _ambient  :: Color,
    _diffuse  :: Color,
    _specular :: Color
} deriving (Show, Read)
makeLenses ''Material

data Shape
    = Sphere {_center :: Point, _radius :: Float, _material :: Material}
    | Plane {_direction :: Vector, _material :: Material}
        deriving (Show, Read)
makeLenses ''Shape

type Scene = [Shape]

instance Traceable Shape where
    intPoint ray Sphere{..} = sphIntersect ray _center _radius
    intPoint ray Plane{..}  = planeIntersect ray _direction
    colorAt shape (Ray ro _) point = attenuate (distance ro point) (shape ^. material . ambient)

attenuate :: Float -> Color -> Color
attenuate dist (V4 r g b a) = V4 r' g' b' a
    where
        k = exp (-0.8 * (dist - 3))
        [r', g', b'] = map (floor . (*k) . fromIntegral) [r, g, b]

sphIntersect :: Ray -> Point -> Float -> Maybe Point
sphIntersect (Ray ro rd) ce ra = if h < 0 then Nothing else Just $ ro ^-^ rd ^* (b + sqrt h)
    where
        oc = ro - ce
        b = dot oc rd
        c = dot oc oc - ra * ra
        h = b * b - c

planeIntersect :: Ray -> Point -> Maybe Point
planeIntersect (Ray ro rd) p = if k < 0 then Nothing else Just $ ro ^+^ rd ^* k
    where
        k = - dot ro p / dot rd p
