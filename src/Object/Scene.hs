module Object.Scene (
    Scene (..),
    defaultScene
) where

import           Common
import           Object.Light
import           Object.Material
import           Object.Primitive

import           Linear

chess_wall = chessboard (V4 1 1 1 1) (V4 0 0 0 1) Diffuse
mirror = uniform (plain (V4 0 0 0 1)) $ Reflection 0.8
white = uniform (plain (V4 1 1 1 1)) $ Reflection 0.8
blue = uniform (plain (V4 0.4 0.4 1 1)) $ Reflection 0.8
blue2 = uniform (plain (V4 0.4 0.4 1 1)) $ Reflection 0.5
pink = uniform (plain (V4 1 0.3 1 1)) $ Reflection 0.4
focus = uniform (plain (V4 1 1 1 0.0)) $ Refraction 1.7

data Scene = Scene [Shape] [PointLight]

defaultShapes :: [Shape]
defaultShapes = [plane zero (V3 0 1 0) chess_wall
            ,plane (V3 5 0 (-5)) (V3 (-1) 0 1) mirror
            ,plane (V3 (-5) 0 (-5)) (V3 1 0 1) mirror
            ,sphere (V3 0 1 0) 1 blue
            ,sphere (V3 0 2.5 0) 0.5 focus
            ,sphere (V3 3 1 (-2)) 1 blue2
            ,sphere (V3 1.5 0.25 (-1)) 0.25 pink
            ,sphere (V3 1.5 2 (-1)) 0.5 white]

defaultLights :: [PointLight]
defaultLights = [PointLight (V3 1 4 4) (3 * (V4 1 1 1 1))]

defaultScene :: Scene
defaultScene = Scene defaultShapes defaultLights
