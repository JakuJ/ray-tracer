module Common where

import           Linear (V3, V4)

type Point = V3 Float
type Direction = V3 Float
type Normal = (Point, Direction)
type Color = V4 Float
