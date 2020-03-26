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

data Scene = Scene [Shape] [PointLight]

-- Random monad

type Rand = State StdGen

randFloat :: Double -> Double -> Rand Double
randFloat from to = (+ from) . (* (to - from)) <$> state random

randomChoice :: [a] -> Rand a
randomChoice lst = (lst !!) . (`mod` length lst) <$> state random

-- Materials

chess_floor :: Material
chess_floor = chessboard (V3 1 1 1) (V3 0 0 0) $ Reflection 0.5

rgb :: Color -> Material
rgb c = uniform (plain c) Diffuse

materials :: [Material]
materials = map rgb [ V3 0.698 0.617 0.851 -- light pastel purple
                    , V3 0.467 0.867 0.467 -- pastel green
                    , V3 0.996 0.42 0.392 -- pastel red
                    , V3 0.467 0.62 0.796 -- dark pastel blue
                    , V3 0.992 0.992 0.596] -- pastel yellow

-- Random spheres

randomSphere :: Rand Shape
randomSphere = do
  x <- randFloat (-5) 5
  z <- randFloat (-20) (-5)
  y <- randFloat 0 1
  mat <- randomChoice materials
  return $ sphere (V3 x y z) y mat

spheres :: [Shape]
spheres = evalState (replicateM 25 randomSphere) $ mkStdGen 1337

defaultShapes :: [Shape]
defaultShapes = ground : lens : spheres
  where
    ground = plane zero (V3 0 1 0) chess_floor
    lens = sphere (V3 0 3 (-5)) 1 $ uniform (plain (V3 1 1 1)) $ Refraction 1.8 0.9

whiteLight :: Point -> PointLight
whiteLight p = PointLight p $ V3 1 1 1
{-# INLINE whiteLight #-}

defaultLights :: [PointLight]
defaultLights = map whiteLight [(V3 (-5) 5 0)
                               ,(V3 (-5) 5 (-10))
                               ,(V3 5 5 0)
                               ,(V3 5 5 (-10))]

defaultScene :: Scene
defaultScene = Scene defaultShapes defaultLights
