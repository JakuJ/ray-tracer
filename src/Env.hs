{-# LANGUAGE TemplateHaskell #-}

module Env where

import           Control.Lens
import           Linear       (V4 (..))
import           Types

data Camera = Camera {
    _position :: Point,
    _front    :: Vector,
    _up       :: Vector
}
makeLenses ''Camera

data Env = Env {
    _imageWidth  :: Int,
    _imageHeight :: Int,
    _camera      :: Camera
}
makeLenses ''Env

defaultEnv :: Env
defaultEnv = Env 800 600 camera
    where
        camera = Camera (V4 0 2 0 1) (V4 0 0 (-1) 0) (V4 0 1 0 0)
