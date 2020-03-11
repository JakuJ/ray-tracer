module Main (main) where

import           Env           (defaultEnv)
import           Linear        (V4 (..), zero)
import           Output        (saveImage)
import           Parser        (parseScene)
import           Raytracer     (render)
import           Shapes        (Material (..), Scene, Shape (..))
import           System.Random

randomPoints2D :: [(Float, Float)]
randomPoints2D = zip (randoms (mkStdGen 123)) (randoms (mkStdGen 456))

white, gray :: Material
white = Material (V4 1 1 1 1) zero zero
gray = Material (V4 0.2 0.2 0.2 1) zero zero

testScene :: Scene
testScene = take 100 [Sphere (V4 (10 * x - 5) (10 * y - 5) 1 1) 0.5 white | (x, y) <- randomPoints2D]

main :: IO ()
main = do
  -- scene <- parseScene "scene.txt"
  let scene = Plane (V4 0 0 1 0) gray : testScene
  saveImage defaultEnv "obraz.bmp" $ render defaultEnv scene
