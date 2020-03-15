module Tracing.Raytracer where

import           Common
import           Env
import           Geometry.Materials
import           Geometry.Primitives
import           Output
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

render :: Primitive a => Env -> [a] -> Image
render env scene = pixelsToImage $ parallelize env (trace scene) rays
    where
        rays = makeRays env

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

tryHit :: Primitive a => Ray -> [a] -> Maybe (Normal, Material)
tryHit ray primitives = do
  (p, dist) <- listToMaybe $ sortOn snd $ filterZipMaybe (distanceTo ray) primitives
  return $ hit ray dist p

trace :: Primitive a => [a] -> Ray -> Color
trace scene = fromMaybe zero . traceRec 16 scene

traceRec :: Primitive a => Int -> [a] -> Ray -> Maybe Color
traceRec 0 _ _ = Nothing
traceRec n scene ray@(Ray ro rd) = do
  ((point, normal), Material color mtype) <- tryHit ray scene
  let
    incoming = fromMaybe background $ case mtype of
      Diffuse -> Nothing
      Reflection f -> if f == 0
        then Nothing
        else traceRec (n - 1) scene $ offset $ Ray point $ reflect rd normal
      Refraction ix -> if color ^. _w == 1
        then Nothing
        else traceRec (n - 1) scene . offset . Ray point =<< refract ix rd normal
    light = V4 1 1 1 1
    in return . clamp $ case mtype of
      Diffuse -> light * color
      Reflection f -> color * (light ^* (1 - f) + incoming ^* f)
      Refraction _ -> alphaBlend color incoming
