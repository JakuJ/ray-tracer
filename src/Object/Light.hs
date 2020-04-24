{-# LANGUAGE ExistentialQuantification #-}
module Object.Light (
  LightSource,
  applyLight,
  Light,
  ambientLight,
  pointLight,
  dirLight
) where

import           Common
import           Object.Material
import           Object.Primitive
import           Tracing.Intersect
import           Tracing.Ray

import           Linear            hiding (point)

class LightSource a where
    applyLight :: Primitive p => [p] -> Point -> Direction -> Direction -> Phong -> a -> Color

newtype AmbientLight = AmbientLight Color
  deriving Show

instance LightSource AmbientLight where
  applyLight _ _ _ _ (Phong color ambient _ _ _) (AmbientLight lightColor) = ambient *^ color * lightColor

calcPhong :: Direction -- to eye
          -> Direction -- to light
          -> Direction -- normal
          -> Phong -- material info
          -> Color
calcPhong toCamera toLight normal (Phong color _ diffuse specular shininess) = if cos_a > 0
  then color ^* (lambertian + highlight)
  else zero
    where
      cos_a = dot normal toLight
      lambertian = diffuse * cos_a
      cos_b = dot toCamera (reflect (negate toLight) normal)
      highlight = specular * cos_b ** shininess

data PointLight = PointLight Point Color
  deriving Show

instance LightSource PointLight where
  applyLight scene point normal toCamera phong (PointLight lightPos lightColor)
    | intensity < 0.05 = zero
    | otherwise = case obscured of
        Nothing -> light
        Just ((colPt, _), _) -> if distance point colPt < distToLight
          then zero
          else light
      where
        distToLight = distance point lightPos
        toLight = (lightPos - point) ^/ distToLight
        obscured = tryHit (offset $ Ray point toLight) scene
        intensity = exp (-0.1 * distToLight)
        ph = calcPhong toCamera toLight normal phong
        light = if ph > 0 then intensity *^ lightColor * ph else zero

data DirLight = DirLight Direction Color
  deriving Show

instance LightSource DirLight where
  applyLight scene point normal toCamera phong (DirLight lightDir lightColor) = case obscured of
    Nothing -> lightColor * ph
    Just _  -> zero
    where
      toLight = negate lightDir
      obscured = tryHit (offset $ Ray point toLight) scene
      ph = calcPhong toCamera toLight normal phong

-- Existential qualification
data Light = forall a. (Show a, LightSource a) => Light a

instance Show Light where
  show (Light l) = show l

instance LightSource Light where
  applyLight a b c d e (Light l) = applyLight a b c d e l

ambientLight :: Color -> Light
ambientLight = Light . AmbientLight

pointLight :: Point -> Color -> Light
pointLight = Light .: PointLight

dirLight :: Direction -> Color -> Light
dirLight = Light .: DirLight