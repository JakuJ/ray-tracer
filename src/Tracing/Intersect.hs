module Tracing.Intersect (
  tryHit,
  offset
) where

import           Common           (Normal)
import           Object.Material  (Material)
import           Object.Primitive (Primitive, distanceTo, hit)
import           Tracing.Ray      (Ray (..))

import           Data.List        (sortOn)
import           Data.Maybe       (listToMaybe)
import           Linear

filterZipMaybe :: (a -> Maybe b) -> [a] -> [(a, b)]
filterZipMaybe _ [] = []
filterZipMaybe f (x:xs) = case f x of
  Just b  -> (x, b) : filterZipMaybe f xs
  Nothing -> filterZipMaybe f xs

offset :: Ray -> Ray
offset (Ray ro rd) = Ray (ro + rd ^* 0.001) rd
{-# INLINE offset #-}

tryHit :: Primitive a => Ray -> [a] -> Maybe (Normal, Material)
tryHit ray primitives = do
  (p, dist) <- listToMaybe $ sortOn snd $ filterZipMaybe (distanceTo ray) primitives
  return $ hit ray dist p
