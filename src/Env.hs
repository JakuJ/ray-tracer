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
defaultEnv = Env 1600 1200 camera
    where
        camera = Camera (V4 0 4 5 1) (V4 0 0 (-1) 0) (V4 0 1 0 0)
