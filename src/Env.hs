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

getLookAt :: Camera -> M44 Float
getLookAt (Camera p f u) = lookAt f p u

data Env = Env {
    _camera :: Camera
}
makeLenses ''Env

defaultEnv :: Env
defaultEnv = Env $ Camera (V3 0 0 0) (V3 0 0 (-1)) (V3 0 1 0)
