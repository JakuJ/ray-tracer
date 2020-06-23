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

import           Linear            hiding (point, trace)

refract :: Double -> Direction -> Direction -> Maybe Direction
refract index i n = if k < 0 then Nothing else Just $ (eta *^ i) ^+^ (n' ^* (eta * cosi' - sqrt k))
  where
    cosi = normalize i `dot` n
    (cosi', eta, n') = if cosi < 0 then (-cosi, 1 / index, n) else (cosi, index, negate n)
    k = 1 - eta * eta * (1 - cosi' * cosi')


-- |For a given 'Env'ironment and a 'Scene', return a list of pixels of the output image in row-major order.
render :: Env -> Scene -> [Color]
render env scene = parallelize (trace scene . Ray pos) $ makeRays env
  where
    pos = _eye $ _camera env

trace :: Scene -> Ray -> Color
trace = traceRec 32

traceRec :: Int -> Scene -> Ray -> Color
traceRec = traceRec' clamp 1
  where
    traceRec' :: (Color -> Color) -> Double -> Int -> Scene -> Ray -> Color
    traceRec' cont _ 0 _ _ = cont zero
    traceRec' cont !intensity n scene@(Scene objs lights) ray@(Ray _ rd)
      | intensity < 0.01 = cont zero
      | otherwise = case tryHit ray objs of
        Nothing -> cont zero
        Just ((point, normal), Material phong mtype) -> let
          direct = sum $ map (applyLight objs point normal (negate rd) phong) lights
          in case mtype of
            Diffuse -> cont direct
            Reflection reflectivity -> let
                cont' next = cont $ lerp reflectivity (next * direct) direct
                ray' = offset $ Ray point $ reflect rd normal
                in traceRec' cont' (intensity * reflectivity) (n - 1) scene ray'
            Refraction ix transmittance -> case refract ix rd normal of
                Nothing -> cont direct
                Just dir -> let
                  cont' next = cont $ lerp transmittance (next * direct) direct
                  ray' = offset $ Ray point dir
                  in traceRec' cont' (intensity * transmittance) (n - 1) scene ray'
