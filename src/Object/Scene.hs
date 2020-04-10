module Object.Scene (
  Scene (..),
  defaultScene
) where

import           Common
import           Object.Light
import           Object.Material
import           Object.Primitive

import           Control.Monad       (replicateM)
import           Control.Monad.State (State, evalState, state)
import           Linear
import           System.Random       (StdGen, mkStdGen, random)

data Scene = Scene [Shape] [Light]

-- Random monad

type Rand = State StdGen

randFloat :: Double -> Double -> Rand Double
randFloat from to = (+ from) . (* (to - from)) <$> state random

randomChoice :: [a] -> Rand a
randomChoice lst = (lst !!) . (`mod` length lst) <$> state random

-- Materials

rgb :: Color -> Material
rgb c = uniform (plain c) Diffuse

materials :: [Material]
materials = map rgb [V3 0.698 0.617 0.851 -- light pastel purple
                    ,V3 0.467 0.867 0.467 -- pastel green
                    ,V3 0.996 0.42 0.392 -- pastel red
                    ,V3 0.467 0.62 0.796 -- dark pastel blue
                    ,V3 0.992 0.992 0.596] -- pastel yellow

-- Random spheres

randomSphere :: Rand Shape
randomSphere = do
  x <- randFloat (-5) 5
  z <- randFloat (-5) 5
  y <- randFloat 0 1
  mat <- randomChoice materials
  return $! sphere (V3 x y z) y mat

spheres :: [Shape]
spheres = evalState (replicateM 25 randomSphere) $ mkStdGen 1337

defaultShapes :: [Shape]
defaultShapes = ground : spheres
  where
    ground = plane zero (V3 0 1 0) $ chessboard (V3 1 1 1) (V3 0 0 0) $ Reflection 0.5
    lens = sphere (V3 0 3 0) 1 $ uniform (plain (V3 1 1 1)) $ Refraction 1.8 0.9

whiteLight :: Point -> Light
whiteLight p = pointLight p $ V3 1 1 1
{-# INLINE whiteLight #-}

whiteDir :: Double -> Direction -> Light
whiteDir int = dirLight <*> const (V3 int int int)

defaultLights :: [Light]
defaultLights = [whiteDir 0.25 (normalize $ V3 (-1) (-1) 0)
                ,whiteDir 0.25 (normalize $ V3 1 (-1) 0)
                ,whiteDir 0.25 (normalize $ V3 0 (-1) (-1))
                ,whiteDir 0.25 (normalize $ V3 0 (-1) 1)]

defaultScene :: Scene
defaultScene = Scene defaultShapes defaultLights
