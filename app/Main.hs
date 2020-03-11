module Main (main) where

import           Env           (defaultEnv)
import           Linear        (V4 (..), zero)
import           Output        (saveImage)
import           Parser        (parseScene)
import           Raytracer     (render)
import           Shapes        (Material (..), Scene, Shape (..))
import           System.Random

randomPoints2D :: [(Float, Float)]
randomPoints2D = zip (randoms (mkStdGen 234)) (randoms (mkStdGen 123))

red, green :: Material
red = Material (V4 255 0 0 255) zero zero
green = Material (V4 0 255 0 255) zero zero

testScene :: Scene
testScene = take 100 [Sphere (V4 (10 * x - 5) (3 + 10 * y - 5) (-4) 1) 0.5 (if (x - 0.5) * (y - 0.5) < 0.0 then red else green) | (x, y) <- randomPoints2D]

main :: IO ()
main = do
  -- scene <- parseScene "scene.txt"
  let scene = testScene
  saveImage defaultEnv "obraz.bmp" $ render defaultEnv scene
