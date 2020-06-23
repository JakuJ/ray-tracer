{-# LANGUAGE BangPatterns #-}
module Tracing.Intersect (
  tryHit,
  offset,
  reflect
) where

import           Common
import           Object.Material  (Material)
import           Object.Primitive (Primitive, distanceTo, hit)
import           Tracing.Ray      (Ray (..))

import           Data.List        (sortOn)
import           Data.Maybe       (listToMaybe)
import           Linear

-- |Offsets the ray by a tiny step in the direction it's being cast.
-- Used to avoid self-colision artifacts when reflecting rays off of surfaces.
offset :: Ray -> Ray
offset (Ray ro rd) = Ray (ro + rd ^* 0.001) rd
{-# INLINE offset #-}

-- |Calculates the direction for a reflected ray based on the incoming ray's direction
-- and the normal vector of the surface at the intersection point.
reflect :: Direction -> Direction -> Direction
reflect !d !n = d - (n ^* (2 * d `dot` n))
{-# INLINE reflect #-}

-- |Casts the provided ray into a scene composed of the provided primitives.
-- Returns 'Nothing' if the ray does not collide with anything.
-- Otherwise returns 'Just' the normal vector at the intersection point
-- and the material info of the primitive the ray collides with first.
tryHit :: Primitive a => Ray -> [a] -> Maybe (Normal, Material)
tryHit ray primitives = do
  (p, dist) <- listToMaybe $ sortOn snd $ filterZipMaybe (distanceTo ray) primitives
  return $ hit ray dist p

filterZipMaybe :: (a -> Maybe b) -> [a] -> [(a, b)]
filterZipMaybe _ [] = []
filterZipMaybe f (x:xs) = case f x of
  Just b  -> (x, b) : filterZipMaybe f xs
  Nothing -> filterZipMaybe f xs
