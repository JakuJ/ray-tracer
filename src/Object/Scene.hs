module Object.Scene where

import           Object.Light
import           Object.Primitive

-- |Represents the scene using a list of primitives and light sources.
data Scene = Scene [Shape] [Light]
