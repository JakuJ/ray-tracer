
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE ExistentialQuantification #-}
module Geometry.Primitives where

import           Common
import           Geometry.Materials
import           Tracing.Ray        (Ray (..))

import           Linear

class HasMaterial a where
  material :: a -> Material

class Primitive_ a where
  distanceTo :: Ray -> a -> Maybe Float
  normalAt :: Point -> a -> Direction
  hit :: Ray -> Float -> a -> (Normal, Material)

  default hit :: HasMaterial a => Ray -> Float -> a -> (Normal, Material)
  hit (Ray ro rd) dist p = ((point, dir), material p)
    where
      point = ro + rd ^* dist
      dir = normalAt point p

data Primitive = forall a. Primitive_ a => Primitive a

instance Primitive_ Primitive where
  distanceTo ray (Primitive a) = distanceTo ray a
  normalAt p (Primitive a) = normalAt p a
  hit ray f (Primitive a) = hit ray f a

-- Implementations

data Sphere = Sphere
  { _spherePosition :: {-# UNPACK #-} !Point
  , _sphereRadius   :: {-# UNPACK #-} !Float
  , _sphereMaterial :: {-# UNPACK #-} !Material
  }

sphere :: Point -> Float -> Material -> Primitive
sphere a b c = Primitive $ Sphere a b c

instance HasMaterial Sphere where
  material = _sphereMaterial

instance Primitive_ Sphere where
  distanceTo (Ray ro rd) (Sphere ce ra _) = if h < 0 then Nothing else closer
    where
      oc = ro - ce
      b = dot oc rd
      c = dot oc oc - ra * ra
      h = b * b - c
      near = -b - sqrt h
      far = -b + sqrt h
      closer = let val = min far near in if val < 0 then Nothing else Just val

  normalAt point (Sphere ce _ _) = normalize $ point - ce

data Plane = Plane
  { _planeDirection :: {-# UNPACK #-} !Direction
  , _planeMaterial  :: {-# UNPACK #-} !Material
  }

plane :: Direction -> Material -> Primitive
plane a b = Primitive $ Plane a b

instance HasMaterial Plane where
  material = _planeMaterial

instance Primitive_ Plane where
  distanceTo (Ray ro rd) (Plane dir _) = if k < 0 || k > 20 then Nothing else Just k
    where
      k = - dot ro dir / dot rd dir

  normalAt point (Plane dir _) = if dot point dir > 0 then dir else negate dir
