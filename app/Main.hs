module Main (main) where

import           Env       (defaultEnv)
import           Linear    (V3 (..))
import           Output    (saveImage)
import           Raytracer (Scene, Shape (..), render)

scene :: Scene
scene = [Sphere (V3 0 2 (-5)) 2, Sphere (V3 3 0 (-5)) 0.5]

main :: IO ()
main = saveImage "obraz.bmp" $ render defaultEnv scene
