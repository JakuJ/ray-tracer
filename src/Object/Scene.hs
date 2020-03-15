module Object.Scene (
    Scene (..),
    defaultScene
) where

import           Common
import           Object.Light
import           Object.Material
import           Object.Primitive

import           Linear

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

data Scene = Scene [Shape] [PointLight]

defaultShapes :: [Shape]
defaultShapes = [plane (V3 0 1 0) chessboard
            ,sphere (V3 0 1 0) 1 blue
            ,sphere (V3 0 2.5 0) 0.5 focus
            ,sphere (V3 3 1 (-2)) 1 blue2
            ,sphere (V3 1.5 0.25 (-1)) 0.25 pink
            ,sphere (V3 1.5 2 (-1)) 0.5 white]

defaultLights :: [PointLight]
defaultLights = [PointLight (V3 3 8 0) (V4 1 1 1 1)
                ,PointLight (V3 (-3) 8 0) (V4 1 1 1 1)]

defaultScene :: Scene
defaultScene = Scene defaultShapes defaultLights
