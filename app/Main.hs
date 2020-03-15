module Main (main) where

import           Common
import           Env               (defaultEnv)
import           Object.Scene      (defaultScene)
import           Output            (saveImage)
import           Tracing.Raytracer (render)

import           Linear            (V3 (..), V4 (..), zero)

main :: IO ()
main = saveImage defaultEnv "obraz.bmp" $ render defaultEnv defaultScene
