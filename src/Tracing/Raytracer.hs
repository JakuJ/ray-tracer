module Tracing.Raytracer (
  render
) where

import           Common
import           Env
import           Object.Light
import           Object.Material
import           Object.Primitive
import           Object.Scene
import           Tracing.Intersect
import           Tracing.Parallel  (parallelize)
import           Tracing.Ray

import           Control.Lens      ((^.))
import           Control.Monad     (guard)

import           Data.List         (sortOn)
import           Data.Maybe        (fromMaybe, listToMaybe)
import           Linear            hiding (trace)

-- |Blend two RGBA 'Color' vectors. If the second one is 'Nothing', treat it as black.
alphaBlend :: Color -> Color -> Color
alphaBlend c1 c2 = V4 r g b out_a
  where
    a1 = c1 ^. _w
    a2 = (c2 ^. _w) * (1 - a1)
    out_a = a1 + a2
    (V3 r g b) = ((c1 ^. _xyz) ^* a1 ^+^ (c2 ^. _xyz) ^* a2) ^/ out_a

reflect :: Direction -> Direction -> Direction
reflect d n = d ^-^ (n ^* (2 * d `dot` n))

refract :: Float -> Direction -> Direction -> Maybe Direction
refract index i n = if k < 0 then Nothing else Just $ (eta *^ i) ^+^ (n' ^* (eta * cosi' - sqrt k))
    where
        cosi = normalize i `dot` n
        (cosi', eta, n') = if cosi < 0 then (-cosi, 1 / index, n) else (cosi, index, negate n)
        k = 1 - eta * eta * (1 - cosi' * cosi')

offset :: Ray -> Ray
offset (Ray ro rd) = Ray (ro + rd ^* 0.0001) rd

clamp :: Color -> Color
clamp = liftI2 min (V4 1 1 1 1) . liftI2 max zero

render :: Env -> Scene -> [Color]
render env scene = parallelize env (trace scene) rays
    where
        rays = makeRays env

trace :: Scene -> Ray -> Color
trace scene = fromMaybe zero . traceRec 16 scene

traceRec :: Int -> Scene -> Ray -> Maybe Color
traceRec 0 _ _ = Nothing
traceRec n scene@(Scene objs lights) ray@(Ray ro rd) = do
  ((point, normal), Material colorAt mtype) <- tryHit ray objs
  let
    color = colorAt point
    incoming = fromMaybe (V4 0 0 0 1) $ case mtype of
      Diffuse -> Nothing
      Reflection f -> if f == 0
        then Nothing
        else traceRec (n - 1) scene $ offset $ Ray point $ reflect rd normal
      Refraction ix -> if color ^. _w == 1
        then Nothing
        else traceRec (n - 1) scene . offset . Ray point =<< refract ix rd normal
    offPoint = point + normal ^* 0.0001
    light = sum $ map (applyLight objs offPoint) lights
    in return . clamp $ case mtype of
      Diffuse      -> light * color
      Reflection f -> color * (light ^* (1 - f) + incoming ^* f)
      Refraction _ -> alphaBlend color incoming
