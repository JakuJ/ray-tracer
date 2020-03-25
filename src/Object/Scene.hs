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
import           Data.Foldable       (toList)
import           Linear
import           System.Random       (Random, StdGen, mkStdGen, random)

data Scene = Scene [Shape] [PointLight]

-- Random monad

type Rand = State StdGen

randFloat :: (Random a, Floating a) => a -> a -> Rand a
randFloat from to = (+ from) . (* (to - from)) <$> state random

randomChoice :: Foldable f => f a -> Rand a
randomChoice lst = (toList lst !!) . (`mod` length lst) <$> state random

-- Materials

chess_floor :: Material
chess_floor = chessboard (V4 1 1 1 1) (V4 0 0 0 1) $ Reflection 0.5

rgb :: Color -> Material
rgb c = uniform (plain c) Diffuse

materials :: [Material]
materials = map rgb [ V4 0.698 0.617 0.851 1 -- light pastel purple
                    , V4 0.467 0.867 0.467 1 -- pastel green
                    , V4 0.996 0.42 0.392 1 -- pastel red
                    , V4 0.467 0.62 0.796 1 -- dark pastel blue
                    , V4 0.992 0.992 0.596 1] -- pastel yellow

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
defaultShapes = plane zero (V3 0 1 0) chess_floor : spheres

whiteLight :: Point -> PointLight
whiteLight p = PointLight p $ V4 1 1 1 1
{-# INLINE whiteLight #-}

defaultLights :: [PointLight]
defaultLights = map whiteLight [(V3 (-5) 5 0)
                               ,(V3 (-5) 5 (-10))
                               ,(V3 5 5 0)
                               ,(V3 5 5 (-10))]

defaultScene :: Scene
defaultScene = Scene defaultShapes defaultLights
