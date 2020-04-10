module Tracing.Ray (
    Ray (..),
    makeRays
) where

import           Common        (Direction)
import           Data.Function (on)
import           Linear

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

makeRays :: Env -> [Direction]
makeRays (Env width height (Camera eye look_at up fov)) = do
  y <- [0 .. height - 1]
  x <- [0 .. width - 1]
  let
    xp = 1 - 2 * (x `fdiv` width)
    yp = 1 - 2 * (y `fdiv` height)
    in return $! normalize $ d *^ w + aspect * xp *^ u + yp *^ v
  where
    w = normalize $ look_at - eye
    u = normalize $ cross up w
    v = normalize $ cross w u
    aspect = width `fdiv` height
    d = 1 / tan (toRadians fov / 2)
