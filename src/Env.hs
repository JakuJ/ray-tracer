{-# LANGUAGE TemplateHaskell #-}

module Env where

import           Control.Lens
import           Linear

data Camera = Camera {
    _position :: V3 Float,
    _front    :: V3 Float,
    _up       :: V3 Float
}
makeLenses ''Camera

data Env = Env {
    _imageWidth  :: Int,
    _imageHeight :: Int,
    _camera      :: Camera
}
makeLenses ''Env

defaultEnv :: Env
defaultEnv = Env 1000 600 camera
    where
        camera = Camera (V3 0 2 0) (V3 0 0 (-1)) (V3 0 1 0)
