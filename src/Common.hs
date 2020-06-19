module Common where

import           Linear

type Point = V3 Double
type Direction = V3 Double
type Normal = (Point, Direction)
type Color = V3 Double

infixr 9 .:
(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = (.) . (.)

infixr 9 .:.
(.:.) :: (d -> e) -> (a -> b -> c -> d) -> (a -> b -> c -> e)
(.:.) = (.) . (.:)

-- |Clamp all of the vector's components into the range 0 - 1
clamp :: Color -> Color
clamp = liftI2 min (V3 1 1 1) . liftI2 max zero
