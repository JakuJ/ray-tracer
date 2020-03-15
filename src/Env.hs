{-# LANGUAGE TemplateHaskell #-}

module Env where

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

-- |Default testing environment
defaultEnv :: Env
defaultEnv = Env 1000 1000 $ Camera (V3 1 3 6) (V3 0 0 (-1)) (V3 0 1 0) 60
