module Tracing.Intersect where

import           Common
import           Object.Material
import           Object.Primitive
import           Tracing.Ray

import           Data.List        (sortOn)
import           Data.Maybe       (listToMaybe)
import           Linear

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

filterZipMaybe :: (a -> Maybe b) -> [a] -> [(a, b)]
filterZipMaybe _ [] = []
filterZipMaybe f (x:xs) = case f x of
  Just b  -> (x, b) : filterZipMaybe f xs
  Nothing -> filterZipMaybe f xs

tryHit :: Primitive a => Ray -> [a] -> Maybe (Normal, Material)
tryHit ray primitives = do
  (p, dist) <- listToMaybe $ sortOn snd $ filterZipMaybe (distanceTo ray) primitives
  return $ hit ray dist p
