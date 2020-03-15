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

render :: Primitive_ a => Env -> [a] -> Image
render env scene = pixelsToImage $ parallelize env (flip trace scene) rays
    where
        rays = makeRays env

reflect :: Direction -> Direction -> Direction
reflect d n = d ^-^ (n ^* (2 * dot d n))

refract :: Float -> Direction -> Direction -> Maybe Direction
refract index i n = if k < 0 then Nothing else Just $ (eta *^ i) ^+^ (n' ^* (eta * cosi' - sqrt k))
    where
        cosi = dot (normalize i) n
        (cosi', eta, n') = if cosi < 0 then (-cosi, 1 / index, n) else (cosi, index, negate n)
        k = 1 - eta * eta * (1 - cosi' * cosi')

offset :: Ray -> Ray
offset (Ray ro rd) = Ray (ro + rd ^* 0.01) rd

tryHit :: Primitive_ a => Ray -> [a] -> Maybe (Normal, Material)
tryHit ray primitives = do
  (p, dist) <- listToMaybe $ sortOn snd $ filterZipMaybe (distanceTo ray) primitives
  return $ hit ray dist p

spawnRays :: Ray -> (Normal, Material) -> [(Ray, (Color -> Maybe Color -> Color))]
spawnRays (Ray ro rd) ((point, normal), (Material col mtype)) = case mtype of
  Diffuse -> []
  Reflection f -> if f == 0 then []
    else let reflected = reflect rd normal in [(offset (Ray point reflected), blend (1 - f))]
  Refraction ix -> if col ^. _w == 1 then []
    else case refract ix rd normal of
      Nothing        -> []
      Just refracted -> [(offset (Ray point refracted), alphaBlend)]

trace :: Primitive_ a => Ray -> [a] -> Color
trace ray = fromMaybe zero . traceRec 16 ray

traceRec :: Primitive_ a => Int -> Ray -> [a] -> Maybe Color
traceRec 0 _ _ = Nothing
traceRec n ray scene = do
  c@(_, Material color _) <- tryHit ray scene
  let other = spawnRays ray c
  return $ foldl (\acc (ray, f) -> f acc (traceRec (n - 1) ray scene)) color other
