module Main (main) where

import           Env           (defaultEnv)
import           Linear        (V4 (..), zero)
import           Output        (saveImage)
import           Parser        (parseScene)
import           Raytracer     (render)
import           Shapes        (Material (..), Scene, Shape (..))
import           System.Random

randomPoints3D :: [(Float, Float, Float)]
randomPoints3D = zip3 (randoms (mkStdGen 123)) (randoms (mkStdGen 456)) (randoms (mkStdGen 789))

white :: Material
white = let w = V4 1 1 1 1 in Material w w w

testScene :: Scene
testScene = do
  x <- [-4, 0, 4]
  z <- [-6, -3, 0]
  return $ Sphere (V4 x 1 z 1) 1 white

main :: IO ()
main = do
  -- scene <- parseScene "scene.txt"
  let scene = Plane (V4 0 1 0 0) white : testScene
  saveImage defaultEnv "obraz.bmp" $ render defaultEnv scene
