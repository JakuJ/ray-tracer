module Main (main) where

import           Env               (defaultEnv)
import           Object.Scene      (defaultScene)
import           Output            (saveImage)
import           Tracing.Raytracer (render)

main :: IO ()
main = saveImage defaultEnv "obraz.bmp" $ render defaultEnv defaultScene
