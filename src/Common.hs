module Common where

import           Linear (V3)

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
