module Object.Scene (
  Scene (..),
  defaultScene
) where

import           Common
import           Object.Light
import           Object.Material
import           Object.Primitive

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
randomChoice lst
  | null lst = error "Cannot select a random element from an empty container"
  | otherwise = (toList lst !!) . (`mod` (length lst)) <$> state random

-- Materials

chess_floor :: Material
chess_floor = chessboard (V4 1 1 1 1) (V4 0 0 0 1) Diffuse

rgb :: Color -> Material
rgb c = uniform (plain c) $ Reflection 0.5

materials :: [Material]
materials = do
  x <- [0, 1]
  y <- [0, 1]
  z <- [0, 1]
  return . rgb $ V4 x y z 1

-- Random spheres

randomSphere :: Rand Shape
randomSphere = do
  x <- randFloat (-5) 5
  z <- randFloat (-20) (-5)
  y <- randFloat 0 1
  mat <- randomChoice materials
  return $ sphere (V3 x y z) y mat

spheres :: [Shape]
spheres = evalState (sequence (replicate 50 randomSphere)) $ mkStdGen 1337

defaultShapes :: [Shape]
defaultShapes = plane zero (V3 0 1 0) chess_floor : spheres

whiteLight :: Point -> PointLight
whiteLight p = PointLight p $ V4 1 1 1 1

defaultLights :: [PointLight]
defaultLights = [whiteLight (V3 (-5) 5 0)
                ,whiteLight (V3 (-5) 5 (-10))
                ,whiteLight (V3 5 5 0)
                ,whiteLight (V3 5 5 (-10))]

defaultScene :: Scene
defaultScene = Scene defaultShapes defaultLights
