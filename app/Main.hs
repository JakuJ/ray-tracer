module Main (main) where

import           Env           (defaultEnv)
import           Linear        (V3 (..), normalize)
import           Output        (saveImage)
import           Parser        (parseScene)
import           Raytracer     (render)
import           Shapes        (Scene, Shape (..))
import           System.Random

randomPoints2D :: [(Float, Float)]
randomPoints2D = zip (randoms (mkStdGen 420)) (randoms (mkStdGen 123))

testScene :: Scene
testScene = take 30 [Sphere (V3 (10 * x - 5) (10 * y - 5) (-7)) 1 | (x, y) <- randomPoints2D]

main :: IO ()
main = do
  -- scene <- parseScene "scene.txt"
  let scene = Plane (normalize (V3 0 1 0)) : testScene
  saveImage defaultEnv "obraz.bmp" $ render defaultEnv scene
