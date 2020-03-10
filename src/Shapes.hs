{-# LANGUAGE LambdaCase #-}

module Shapes (
    collide,
    Ray (..),
    Scene,
    Shape(..)
) where

import           Linear (V3 (..), dot)

type Scene = [Shape]

data Ray = Ray {_pos :: V3 Float, _dir :: V3 Float}

data Shape 
    = Sphere {_center :: V3 Float, _radius :: Float}
    deriving (Show, Read)

collide :: Ray -> Shape -> Maybe Float
collide ray = \case
    Sphere c r -> sphIntersect ray c r

sphIntersect :: Ray -> V3 Float -> Float -> Maybe Float
sphIntersect (Ray ro rd) ce ra = if h < 0 then Nothing else Just $ -b - sqrt h
    where
        oc = ro - ce
        b = dot oc rd
        c = dot oc oc - ra * ra
        h = b * b - c
