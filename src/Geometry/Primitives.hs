
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE ExistentialQuantification #-}

module Geometry.Primitives (
  Primitive,
  distanceTo, normalAt, hit,
  -- * Existential qualifiers
  Shape,
  -- ** Smart constructors
  sphere, plane
) where

import           Common
import           Geometry.Materials
import           Tracing.Ray        (Ray (..))

import           Linear

-- |Represents types that have a 'Material' associated with them.
class HasMaterial a where
  material :: a -> Material -- ^Extract the associated 'Material'.

-- |Represents types that can be intersected by rays
class Primitive a where
  -- |'Just' distance to the primitive if the ray intersects it, otherwise 'Nothing'.
  distanceTo :: Ray -> a -> Maybe Float
  -- |Surface normal at a given point in space.
  normalAt :: Point -> a -> Direction
  -- |Collision point, normal vector and material info. Assumes intersection.
  hit :: Ray -> Float -> a -> (Normal, Material) 

  default hit :: HasMaterial a => Ray -> Float -> a -> (Normal, Material)
  hit (Ray ro rd) dist p = ((point, dir), material p)
    where
      point = ro + rd ^* dist
      dir = normalAt point p

-- |Existential qualifier for the 'Primitive' class.
-- Used to achieve dynamic dispatch known from OOP languages.
-- Makes representing the scene as a list of 'Primitive' class objects possible.
data Shape = forall a. Primitive a => Shape a

instance Primitive Shape where
  distanceTo ray (Shape a) = distanceTo ray a
  normalAt p (Shape a) = normalAt p a
  hit ray f (Shape a) = hit ray f a

data Sphere = Sphere
  { _spherePosition :: {-# UNPACK #-} !Point
  , _sphereRadius   :: {-# UNPACK #-} !Float
  , _sphereMaterial :: {-# UNPACK #-} !Material
  }

-- |A smart constructor upcasting the 'Sphere' type to 'Shape'.
sphere :: Point -> Float -> Material -> Shape
sphere a b c = Shape $ Sphere a b c

instance HasMaterial Sphere where
  material = _sphereMaterial

instance Primitive Sphere where
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

-- |A smart constructor upcasting the 'Plane' type to 'Shape'.
plane :: Direction -> Material -> Shape
plane a b = Shape $ Plane a b

instance HasMaterial Plane where
  material = _planeMaterial

instance Primitive Plane where
  distanceTo (Ray ro rd) (Plane dir _) = if k < 0 || k > 20 then Nothing else Just k
    where
      k = - dot ro dir / dot rd dir

  normalAt point (Plane dir _) = if dot point dir > 0 then dir else negate dir
