{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Env (
    Env (..),
    Camera (..),
    defaultEnv
) where

import           Common (Direction, Point)

import           Linear (V3 (..))

-- |Represents the camera used for rendering.
data Camera = Camera {
    _eye    :: Point, -- ^Camera position in world space
    _lookAt :: Point, -- ^Camera look-at point
    _up     :: Direction, -- ^Camera up vector
    _fov    :: {-# UNPACK #-} !Double -- ^Field of view in degrees
}

-- |Holds environmental constants.
data Env = Env {
    _imageWidth  :: {-# UNPACK #-} !Int, -- ^Image width in pixels
    _imageHeight :: {-# UNPACK #-} !Int, -- ^Image height in pixels
    _camera      :: Camera -- ^The global camera object
}

resEnv :: Int -> Int -> Env
resEnv w h = Env w h $ Camera (V3 0 0 (-20)) (V3 0 0 0) (V3 0 1 0) 60

hd, fullHD, res2k, res4k :: Env
hd = resEnv 720 480
fullHD = resEnv 1920 1080
res2k = resEnv 2560 1600
res4k = resEnv 3840 2160

-- |Default environment, used in case the scene descripton file doesn't specify one.
defaultEnv :: Env
defaultEnv = res2k
