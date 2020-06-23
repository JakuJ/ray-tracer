module Common where

import           Linear

-- |Helper type used to represent points in 3D space.
type Point = V3 Double

-- |Helper type used to represent 3D directional vectors.
type Direction = V3 Double

-- |Helper type used to a normal vector at a point.
type Normal = (Point, Direction)

-- |Helper type used to represent RGB colours.
type Color = V3 Double

infixr 9 .:
-- |Function composition, one level above '.'.
-- Defined as (.) . (.)
(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = (.) . (.)

infixr 9 .:.
-- |Function composition, two levels above '.'.
-- Defined as (.) . (.:)
(.:.) :: (d -> e) -> (a -> b -> c -> d) -> (a -> b -> c -> e)
(.:.) = (.) . (.:)

-- |Clamps all of the vector's components into the range 0-1
clamp :: Color -> Color
clamp = liftI2 min (V3 1 1 1) . liftI2 max zero
