module Tracing.Raytracer where

import           Common
import           Env
import           Object.Light
import           Object.Material
import           Object.Primitive
import           Object.Scene
import           Output
import           Tracing.Intersect
import           Tracing.Ray
import           Utils

import           Control.Lens                ((^.))
import           Control.Monad               (guard)
import           Control.Parallel.Strategies (parListChunk, rseq, withStrategy)
import           Data.List                   (sortOn)
import           Data.Maybe                  (fromMaybe, listToMaybe)
import           Linear                      hiding (trace)

parallelize :: Env -> (a -> b) -> [a] -> [b]
parallelize env f = withStrategy (parListChunk chunkSize rseq) . map f
    where
        chunkSize = env ^. imageWidth * env ^. imageHeight `div` 40

render :: Env -> Scene -> Image
render env scene = pixelsToImage $ parallelize env (trace scene) rays
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
    incoming = fromMaybe background $ case mtype of
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
