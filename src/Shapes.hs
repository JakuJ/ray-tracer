{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Shapes where

import           Control.Lens
import           Linear

import           Materials    (Color, Material, color)
import           Ray

type Point = V3 Float
type Vector = V3 Float

data Shape
    = Sphere {_center :: Point, _radius :: Float, _material :: Material}
    | Plane {_direction :: Vector, _material :: Material}
        deriving (Show, Read)
makeLenses ''Shape

data Collision = Collision {
    _shape    :: Shape,
    _distance :: Float,
    _point    :: Point,
    _normal   :: Vector
}

normalAt :: Shape -> Point -> Vector
normalAt (Sphere c _ _) point = normalize $ point ^-^ c
normalAt (Plane d _) point = if dot d (normalize point) > 0 then d else negate d

colorAt :: Shape -> Point -> Color
colorAt (Sphere _ _ m) _ = m ^. color
colorAt (Plane _ m) (V3 x y z)
    | even (floor (5 * x)) /= even (floor (5 * y)) = (m ^. color) * V4 0 0 0 1
    | otherwise = (m ^. color) * V4 1 1 1 1

collide :: Ray -> Shape -> Maybe Collision
collide ray@(Ray ro rd) shp = do
    dist <- intersect ray shp
    let point = ro ^+^ rd ^* dist
    return $ Collision shp dist point $ normalAt shp point

intersect :: Ray -> Shape -> Maybe Float
intersect ray@(Ray ro rd) shp@(Sphere ce ra _) = if h < 0 then Nothing else closer
    where
        oc = ro - ce
        b = dot oc rd
        c = dot oc oc - ra * ra
        h = b * b - c
        near = -b - sqrt h
        far = -b + sqrt h
        closer = let val = min far near in if val < 0 then Nothing else Just val

intersect ray@(Ray ro rd) shp@(Plane dir _) = if k < 0 then Nothing else Just k
    where
        k = - dot ro dir / dot rd dir
