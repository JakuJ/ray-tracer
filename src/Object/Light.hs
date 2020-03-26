module Object.Light where

import           Common
import           Object.Material
import           Object.Primitive
import           Tracing.Intersect
import           Tracing.Ray

import           Linear            hiding (point)

class Light a where
    applyLight :: Primitive p => [p] -> Point -> Direction -> Direction -> Phong -> a -> Color

data PointLight = PointLight Point Color

instance Light PointLight where
  applyLight scene point normal toCamera (Phong color _ diffuse specular shininess) (PointLight lightPos lightColor) = case obscured of
    Nothing -> light
    Just ((colPt, _), _) -> if distance point colPt < distToLight
      then zero
      else light
    where
      toLight = normalize $ lightPos - point
      obscured = tryHit (offset $ Ray point toLight) scene
      distToLight = distance point lightPos
      intensity = exp (-0.1 * distToLight)
      cos_a = dot normal toLight
      lambertian = diffuse * cos_a
      cos_b = dot toCamera (reflect (negate toLight) normal)
      highlight = specular * cos_b ** shininess
      light = if cos_a > 0 then lightColor * color ^* (intensity * (lambertian + highlight)) else zero
