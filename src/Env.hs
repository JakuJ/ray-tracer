{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Env (
    Env (..),
    Camera (..),
    defaultEnv
) where

import           Linear (V3 (..))

-- |Represents the camera used for the rendering
data Camera = Camera {
    _position :: V3 Double, -- ^Camera position in world space
    _front    :: V3 Double, -- ^Camera direction, normalized vector
    _up       :: V3 Double, -- ^Camera up vector
    _fov      :: {-# UNPACK #-} !Double     -- ^Field of view in degrees
}

-- |Holds environmental constants
data Env = Env {
    _imageWidth  :: {-# UNPACK #-} !Int, -- ^Image width in pixels
    _imageHeight :: {-# UNPACK #-} !Int, -- ^Image height in pixels
    _camera      :: Camera -- ^The global camera object
}

resEnv :: Int -> Int -> Env
resEnv w h = Env w h $ Camera (V3 0 3 0) (V3 0 0 (-1)) (V3 0 1 0) 60

hd, fullHD, res2k, res4k :: Env
hd = resEnv 720 480
fullHD = resEnv 1920 1080
res2k = resEnv 2560 1600
res4k = resEnv 3840 2160

-- |Default testing environment
defaultEnv :: Env
defaultEnv = fullHD
