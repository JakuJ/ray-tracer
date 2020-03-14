module Raytracer where

import           Control.Lens                ((^.))
import           Control.Monad               (guard)
import           Control.Parallel.Strategies (parListChunk, rseq, withStrategy)
import           Data.List                   (sortOn)
import           Data.Maybe                  (fromMaybe, listToMaybe, mapMaybe)
import           Data.Ord                    (comparing)
import           Linear                      hiding (trace)

import           Env
import           Materials
import           Output
import           Ray
import           Shapes

parallelize :: Env -> (a -> b) -> [a] -> [b]
parallelize env f = withStrategy (parListChunk chunkSize rseq) . map f
    where
        chunkSize = env ^. imageWidth * env ^. imageHeight `div` 40

render :: Env -> [Shape] -> Image
render env scene = pixelsToImage $ parallelize env (flip trace scene) rays
    where
        rays = makeRays env

reflect :: Vector -> Vector -> Vector
reflect d n = d ^-^ (n ^* (2 * dot d n))

refract :: Float -> Vector -> Vector -> Maybe Vector
refract index i n = if k < 0 then Nothing else Just $ (eta *^ i) ^+^ (n' ^* (eta * cosi' - sqrt k))
    where
        cosi = dot (normalize i) n
        (cosi', eta, n') = if cosi < 0 then (-cosi, 1 / index, n) else (cosi, index, negate n)
        k = 1 - eta * eta * (1 - cosi' * cosi')

trace :: Ray -> [Shape] -> Color
trace ray = fromMaybe zero . traceRec 16 ray

traceRec :: Int -> Ray -> [Shape] -> Maybe Color
traceRec 0 _ _ = Nothing
traceRec n ray@(Ray ro rd) scene = do
    let intersections = mapMaybe (collide ray) scene
    (Collision shape dist point normal) <- listToMaybe $ sortOn _distance intersections
    let color = colorAt shape point

    -- Reflection
    let withReflection = blend (1 - shape ^. material . reflectivity) color $ do
            guard $ shape ^. material . reflectivity > 0
            let reflected = reflect rd normal
            let newRay = Ray (point ^+^ (reflected ^* 0.01)) reflected
            traceRec (n - 1) newRay scene

    -- Refraction
    return $ alphaBlend withReflection $ do
        guard $ withReflection ^. _w < 1
        refracted <- refract (shape ^. material . refractiveIndex) rd normal
        let newRay = Ray (point ^+^ (refracted ^* 0.01)) refracted
        traceRec (n - 1) newRay scene
