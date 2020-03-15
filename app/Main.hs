module Main (main) where

import           Common
import           Env                 (defaultEnv)
import           Geometry.Materials
import           Geometry.Primitives
import           Output              (saveImage)
import           Tracing.Raytracer   (render)

import           Linear              (V3 (..), V4 (..), zero)

plain :: Color -> MaterialType -> Material
plain color = Material (const color)

chess :: Point -> Color
chess (V3 x _ z) = if (even . floor) x == (even . floor) z then V4 1 1 1 1 else V4 0 0 0 1

chessboard = Material chess $ Reflection 0.2
white = plain (V4 1 1 1 1) $ Reflection 0.8
blue = plain (V4 0.4 0.4 1 1) $ Reflection 0.8
blue2 = plain (V4 0.4 0.4 1 1) $ Reflection 0.5
pink = plain (V4 1 0.3 1 1) $ Reflection 0.4
focus = plain (V4 1 1 1 0.1) $ Refraction 1.7

testScene :: [Shape]
testScene = [plane (V3 0 1 0) chessboard
            ,sphere (V3 0 1 0) 1 blue
            ,sphere (V3 0 2.5 0) 0.5 focus
            ,sphere (V3 3 1 (-2)) 1 blue2
            ,sphere (V3 1.5 0.25 (-1)) 0.25 pink
            ,sphere (V3 1.5 2 (-1)) 0.5 white]

main :: IO ()
main = saveImage defaultEnv "obraz.bmp" $ render defaultEnv testScene
