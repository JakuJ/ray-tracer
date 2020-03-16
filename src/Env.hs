{-# LANGUAGE TemplateHaskell #-}

module Env (
    Env (..),
    Camera (..),
    defaultEnv,
    -- * Lenses
    imageWidth,
    imageHeight
) where

import           Control.Lens (makeLenses)
import           Linear       (V3 (..))

-- |Represents the camera used for the rendering
data Camera = Camera {
    _position :: V3 Float, -- ^Camera position in world space
    _front    :: V3 Float, -- ^Camera direction, normalized vector
    _up       :: V3 Float, -- ^Camera up vector
    _fov      :: Float     -- ^Field of view in degrees
}
makeLenses ''Camera

-- |Holds environmental constants
data Env = Env {
    _imageWidth  :: Int, -- ^Image width in pixels
    _imageHeight :: Int, -- ^Image height in pixels
    _camera      :: Camera -- ^The global camera object
}
makeLenses ''Env

resEnv :: Int -> Int -> Env
resEnv w h = Env w h $ Camera (V3 1 3 6) (V3 0 0 (-1)) (V3 0 1 0) 60

hd, fullHD, res2k, res4k :: Env
hd = resEnv 720 480
fullHD = resEnv 1920 1080
res2k = resEnv 2560 1600
res4k = resEnv 3840 2160

-- |Default testing environment
defaultEnv :: Env
defaultEnv = res2k
