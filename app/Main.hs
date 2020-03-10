module Main (main) where

import           Env       (defaultEnv)
import           Output    (saveImage)
import           Parser    (parseScene)
import           Raytracer (render)

main :: IO ()
main = do
  scene <- parseScene "scene.txt"
  saveImage defaultEnv "obraz.bmp" $ render defaultEnv scene
