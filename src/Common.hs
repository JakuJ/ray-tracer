module Common where

import           Linear (V3)

type Point = V3 Double
type Direction = V3 Double
type Normal = (Point, Direction)
type Color = V3 Double

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = (.) . (.)

(.:.) :: (d -> e) -> (a -> b -> c -> d) -> (a -> b -> c -> e)
(.:.) = (.) . (.:)
