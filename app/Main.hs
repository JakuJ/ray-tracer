module Main (main) where

import           Env                 (defaultEnv)
import           Geometry.Materials
import           Geometry.Primitives
import           Output              (saveImage)
import           Tracing.Raytracer   (render)

import           Linear              (V3 (..), V4 (..), zero)

plane_white = Material (V4 1 1 1 1) $ Reflection 0.2
white = Material (V4 1 1 1 1) $ Reflection 0.8
blue = Material (V4 0.4 0.4 1 1) $ Reflection 0.8
blue2 = Material (V4 0.4 0.4 1 1) $ Reflection 0.5
pink = Material (V4 1 0.3 1 1) $ Reflection 0.4

testScene :: [Shape]
testScene = [plane (V3 0 1 0) plane_white
            ,sphere (V3 0 1 0) 1 blue
            ,sphere (V3 3 1 (-2)) 1 blue2
            ,sphere (V3 1.5 0.25 (-1)) 0.25 pink
            ,sphere (V3 1.5 2 (-1)) 0.5 white]

main :: IO ()
main = saveImage defaultEnv "obraz.bmp" $ render defaultEnv testScene
