module Main (main) where

import           Env                 (defaultEnv)
import           Geometry.Materials
import           Geometry.Primitives
import           Output              (saveImage)
import           Tracing.Raytracer   (render)

import           Linear              (V3 (..), V4 (..), zero)

white = Material (V4 1 1 1 1) Diffuse
blue = Material (V4 0.1 0.03 0.73 1) $ Reflection 0.6
pink = Material (V4 1 0.1 0.6 1) $ Reflection 0.6
silver = Material (V4 0.25 0.25 0.25 1) $ Reflection 0.8
transparent = Material (V4 0 0 0 0.4) $ Refraction 1.5

testScene :: [Shape]
testScene = [plane (V3 0 1 0) white
            ,sphere (V3 0 1 0) 1 blue
            ,sphere (V3 3 1 (-2)) 1 blue
            ,sphere (V3 1.5 0.25 (-1)) 0.25 pink
            ,sphere (V3 1.5 2 (-1)) 0.5 silver]

main :: IO ()
main = saveImage defaultEnv "obraz.bmp" $ render defaultEnv testScene
