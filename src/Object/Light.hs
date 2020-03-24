module Object.Light where

import           Common
import           Object.Material
import           Object.Primitive
import           Tracing.Intersect
import           Tracing.Ray

import           Linear            hiding (point)

class Light a where
    applyLight :: Primitive p => [p] -> Normal -> Phong -> a -> Color

data PointLight = PointLight Point Color

instance Light PointLight where
  applyLight scene (point, normal) (Phong _ diffuse _) (PointLight lightPos color) = case obscured of
    Just ((colPt, _), _) -> if distance point colPt < distToLight
      then zero
      else light
    Nothing -> light
    where
      distToLight = distance point lightPos
      dirToLight = normalize $ lightPos - point
      obscured = tryHit (offset $ Ray point dirToLight) scene
      intensity = exp (-0.1 * distToLight)
      cos_a = dot normal dirToLight
      light = if cos_a > 0 then diffuse * color ^* (intensity * cos_a) else zero
