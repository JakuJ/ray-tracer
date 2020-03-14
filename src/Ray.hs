module Ray (
    Ray (..),
    makeRays
) where

import           Control.Lens  ((^.))
import           Data.Function (on)
import           Linear        (V3 (..), normalize)

import           Env

data Ray = Ray {
    _pos :: V3 Float,
    _dir :: V3 Float
}

-- | Divide with cast to float
castDiv :: Integral a => a -> a -> Float
castDiv = (/) `on` fromIntegral

toRadians :: Floating a => a -> a
toRadians degs = pi * degs / 180

-- TODO: Actually use the camera
makeRays :: Env -> [Ray]
makeRays (Env width height (Camera position _ _ fov)) = do
    y <- [0 .. height - 1]
    x <- [0 .. width - 1]
    let nx = 2 * (x `castDiv` width) - 1
    let ny = 2 * (y `castDiv` height) - 1
    let dir = normalize $ V3 (aspect * nx * tan_a) (ny * tan_a) (-1)
    return $ Ray position dir
    where
        aspect = width `castDiv` height
        tan_a = tan $ toRadians fov / 2 

