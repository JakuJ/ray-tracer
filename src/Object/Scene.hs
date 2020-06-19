module Object.Scene where

import           Object.Light
import           Object.Primitive

data Scene = Scene [Shape] [Light]