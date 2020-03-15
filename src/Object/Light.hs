module Object.Light where

import           Common
import           Object.Material
import           Object.Primitive
import           Tracing.Intersect
import           Tracing.Ray

import           Linear

class Light a where
    applyLight :: Primitive p => [p] -> Point -> a -> Color

data PointLight = PointLight Point Color

instance Light PointLight where
  applyLight scene point (PointLight lightPos color) = case obscured of
    Just ((colPt, _), _) -> if distance point colPt < distToLight
      then V4 0 0 0 1
      else light
    Nothing -> light
    where
      distToLight = distance point lightPos
      dirToLight = normalize $ lightPos - point
      obscured = tryHit (Ray point dirToLight) scene
      light = 5 *^ color ^/ distToLight
