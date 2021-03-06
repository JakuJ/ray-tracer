{-# LANGUAGE ExistentialQuantification #-}
module Object.Light (
  LightSource,
  applyLight,
  -- * Existential qualifiers
  Light,
  -- ** Smart constructors
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

-- |An abstraction over types which can represent light sources.
class LightSource a where
  applyLight :: Primitive p => [p] -> Point -> Direction -> Direction -> Phong -> a -> Color
  -- ^Calculates the resulting RGB colour based on the scene, the point in space and material properties.

newtype AmbientLight = AmbientLight Color

instance LightSource AmbientLight where
  applyLight _ _ _ _ (Phong color _) (AmbientLight lightColor) = color * lightColor

calcPhong :: Direction -- to eye
          -> Direction -- to light
          -> Direction -- normal
          -> Phong -- material info
          -> Color
calcPhong toCamera toLight normal (Phong color shininess) = if lambertian > 0
  then color ^* (lambertian + highlight)
  else zero
    where
      lambertian = dot normal toLight
      cos_b = dot toCamera (reflect (negate toLight) normal)
      highlight = if shininess == 0 then 0 else cos_b ** shininess

data PointLight = PointLight Point Color

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

instance LightSource DirLight where
  applyLight scene point normal toCamera phong (DirLight lightDir lightColor) = case obscured of
    Nothing -> lightColor * ph
    Just _  -> zero
    where
      toLight = negate lightDir
      obscured = tryHit (offset $ Ray point toLight) scene
      ph = calcPhong toCamera toLight normal phong

-- |Existential qualifier for the 'LightSource' class.
-- Used to achieve dynamic dispatch known from OOP languages.
-- Makes is possible to use a list of anything that implements the 'LightSource' class possible,
-- regardless of the actual data type underneath.
data Light = forall a. LightSource a => Light a

instance LightSource Light where
  applyLight a b c d e (Light l) = applyLight a b c d e l

-- |A smart constructor for ambient lights.
-- Takes in the colour of the light source.
ambientLight :: Color -> Light
ambientLight = Light . AmbientLight

-- |A smart constructor for point lights.
-- Takes in the position and the colour of the light source.
pointLight :: Point -> Color -> Light
pointLight = Light .: PointLight

-- |A smart constructor for directional lights.
-- Takes in the direction and the colour of the light source.
dirLight :: Direction -> Color -> Light
dirLight = Light .: DirLight
