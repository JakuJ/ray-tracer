{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Shapes where

import           Control.Lens
import           Linear
import           Types

data Ray = Ray {_pos :: Point, _dir :: Vector}

class Traceable a where
    colorAt :: a -> Point -> Color
    normalAt :: a -> Point -> Vector
    collide :: Ray -> a -> Maybe (Collision a)
    {-# MINIMAL colorAt, normalAt, collide #-}

data (Traceable a) => Collision a = Collision {
    _ray      :: Ray,
    _shape    :: a,
    _distance :: Float
}

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
    collide ray shp = Collision ray shp <$> intersect ray shp
    normalAt (Sphere c _ _) point = normalize $ point ^-^ c
    normalAt (Plane d _) _ = d
    colorAt (Sphere c r m) point = (m ^. ambient) * (abs $ normalize (point ^-^ c) & _w .~ 1)
    colorAt (Plane _ m) (V4 x y z _)
        | even (floor x) /= even (floor z) = (m ^. ambient) * V4 0.2 0.2 0.2 1
        | otherwise = (m ^. ambient) * V4 0.8 0.8 0.8 1

intersect :: Ray -> Shape -> Maybe Float
intersect ray@(Ray ro rd) shp@(Sphere ce ra _) = if h < 0 || dist < 0 then Nothing else Just dist
    where
        oc = ro - ce
        b = dot oc rd
        c = dot oc oc - ra * ra
        h = b * b - c
        dist = -b - sqrt h

intersect ray@(Ray ro rd) shp@(Plane dir _) = if k < 0 || k > 20 then Nothing else Just k
    where
        k = - dot ro dir / dot rd dir
