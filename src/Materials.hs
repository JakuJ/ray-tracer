{-# LANGUAGE TemplateHaskell #-}

module Materials (
    Material (..),
    Color,
    color, refractiveIndex, reflectivity,
    alphaBlend, blend
) where

import           Control.Lens (makeLenses, (^.))
import           Data.Maybe   (fromMaybe)
import           Linear

type Color = V4 Float

data Material = Material {
    _color           :: Color,
    _reflectivity    :: Float,
    _refractiveIndex :: Float
} deriving (Show, Read)
makeLenses ''Material

alphaBlend :: Color -> Maybe Color -> Color
alphaBlend c1 (Just c2) = V4 r g b out_a
    where
        a1 = c1 ^. _w
        a2 = (c2 ^. _w) * (1 - a1)
        out_a = a1 + a2
        (V3 r g b) = ((c1 ^. _xyz) ^* a1 ^+^ (c2 ^. _xyz) ^* a2) ^/ out_a

alphaBlend c1 Nothing = alphaBlend c1 $ Just (V4 0 0 0 1)

blend :: Float -> Color -> Maybe Color -> Color
blend k c1 (Just c2) = lerp k c1 c2
blend _ c1 Nothing = c1
