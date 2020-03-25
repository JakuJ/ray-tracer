{-# LANGUAGE BangPatterns #-}

module Tracing.Raytracer (
  render
) where

import           Common
import           Env
import           Object.Light
import           Object.Material
import           Object.Scene
import           Tracing.Intersect
import           Tracing.Parallel  (parallelize)
import           Tracing.Ray

import           Data.Maybe        (fromMaybe)
import           Linear            hiding (point, trace)

rgb :: Color -> V3 Double
rgb (V4 r g b _) = V3 r g b
{-# INLINE rgb #-}

alpha :: Color -> Double
alpha (V4 _ _ _ a) = a
{-# INLINE alpha #-}

-- |Blend two RGBA 'Color' vectors. If the second one is 'Nothing', treat it as black.
alphaBlend :: Color -> Color -> Color
alphaBlend !c1 !c2 = V4 r g b out_a
  where
    a1 = alpha c1
    a2 = alpha c2 * (1 - a1)
    out_a = a1 + a2
    (V3 r g b) = ((rgb c1) ^* a1 ^+^ (rgb c2) ^* a2) ^/ out_a

reflect :: Direction -> Direction -> Direction
reflect !d !n = d ^-^ (n ^* (2 * d `dot` n))
{-# INLINE reflect #-}

refract :: Double -> Direction -> Direction -> Maybe Direction
refract index i n = if k < 0 then Nothing else Just $ (eta *^ i) ^+^ (n' ^* (eta * cosi' - sqrt k))
  where
    cosi = normalize i `dot` n
    (cosi', eta, n') = if cosi < 0 then (-cosi, 1 / index, n) else (cosi, index, negate n)
    k = 1 - eta * eta * (1 - cosi' * cosi')

clamp :: Color -> Color
clamp = liftI2 min (V4 1 1 1 1) . liftI2 max zero

render :: Env -> Scene -> [Color]
render env scene = parallelize (trace scene . Ray pos) $ makeRays env
  where
    pos = _position . _camera $ env

trace :: Scene -> Ray -> Color
trace = fromMaybe zero .: traceRec 32

traceRec :: Int -> Scene -> Ray -> Maybe Color
traceRec 0 _ _ = Nothing
traceRec !n scene@(Scene objs lights) ray@(Ray _ rd) = do
  ((point, normal), Material colorAt mtype) <- tryHit ray objs
  let
    phong = colorAt point
    local = sum $ map (applyLight objs (point, normal) phong) lights

    incoming = case mtype of
      Diffuse -> Nothing
      Reflection f -> if f == 0
        then Nothing
        else traceRec (n - 1) scene $ offset $ Ray point $ reflect rd normal
      Refraction ix -> if alpha local == 1
        then Nothing
        else traceRec (n - 1) scene . offset . Ray point =<< refract ix rd normal

    in return . clamp $ case mtype of
      Diffuse      -> local
      Reflection f -> case incoming of
        Just col -> lerp f col local
        Nothing  -> local
      Refraction _ -> alphaBlend local (fromMaybe (V4 0 0 0 1) incoming)
