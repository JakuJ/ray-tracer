module Tracing.Ray (
    Ray (..),
    makeRays
) where

import           Common        (Direction)
import           Data.Function (on)
import           Linear        (V3 (..), normalize)

import           Env

data Ray = Ray {
  _rayOrigin    :: V3 Double,
  _rayDirection :: V3 Double
}

-- | Divide with cast to float
fdiv :: (Integral a, Fractional b) => a -> a -> b
fdiv = (/) `on` fromIntegral

toRadians :: Floating a => a -> a
toRadians = (* pi) . (/ 180)

-- TODO: Actually use the camera
makeRays :: Env -> [Direction]
makeRays (Env width height (Camera _ _ _ fov)) = do
  y <- [height - 1, height - 2 .. 0]
  x <- [0 .. width - 1]
  let
    nx = 2 * (x `fdiv` width) - 1
    ny = 2 * (y `fdiv` height) - 1
    in return $! normalize $ V3 (aspect * nx * tan_a) (ny * tan_a) (-1)
  where
    aspect = width `fdiv` height
    tan_a = tan $ toRadians fov / 2
