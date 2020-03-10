{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Raytracer where

import           Control.Lens
import           Data.Function (on)
import           Data.Maybe    (listToMaybe, mapMaybe)
import           Env
import           Linear        hiding (trace)
import           Output

data Shape = Sphere {_center :: V3 Float, _radius :: Float}

type Scene = [Shape]

data Ray = Ray {_pos :: V3 Float, _dir :: V3 Float}

castDiv :: Integral a => a -> a -> Float
castDiv = (/) `on` fromIntegral

-- TODO: Actually use the camera
makeRays :: Camera -> Int -> Int -> [Ray]
makeRays camera width height = do
    y <- [0 .. height - 1]
    x <- [0 .. width - 1]
    let nx = 2 * (x `castDiv` width) - 1
    let ny = 2 * (y `castDiv` height) - 1
    return $ Ray (camera ^. position) (normalize (V3 nx ny (-1)))

render :: Env -> Scene -> Image
render env scene = pixelsToImage $ map (trace scene) rays
    where
        rays = makeRays (env ^. camera) imageWidth imageHeight

trace :: Scene -> Ray -> Pixel
trace scene ray = color $ listToMaybe $ collisions ray scene
    where
        color Nothing  = V4 0 0 0 255
        color (Just d) = let k = floor (20 * d) in V4 k k k 255

collisions :: Ray -> Scene -> [Float]
collisions ray = mapMaybe (collide ray)

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
