
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE ExistentialQuantification #-}

module Object.Primitive (
  Primitive,
  distanceTo, normalAt, hit,
  -- * Existential qualifiers
  Shape,
  -- ** Smart constructors
  sphere, plane
) where

import           Common
import           Object.Material
import           Tracing.Ray     (Ray (..))

import           Linear

class HasMaterial a where
  material :: a -> Material

-- |Represents types that can be intersected by rays
class Primitive a where
  -- |'Just' distance to the primitive if the ray intersects it, otherwise 'Nothing'.
  distanceTo :: Ray -> a -> Maybe Double
  -- |Surface normal at a given point in space.
  normalAt :: Point -> a -> Direction
  -- |Collision point, normal vector and material info. Assumes intersection.
  hit :: Ray -> Double -> a -> (Normal, Material)

  default hit :: HasMaterial a => Ray -> Double -> a -> (Normal, Material)
  hit (Ray ro rd) dist p = ((pt, dir), material p)
    where
      pt = ro + rd ^* dist
      dir = normalAt pt p

data Sphere = Sphere
  { _spherePosition :: Point
  , _sphereRadius   :: {-# UNPACK #-} !Double
  , _sphereMaterial :: Material
  }

instance HasMaterial Sphere where
  {-# INLINE material #-}
  material = _sphereMaterial

instance Primitive Sphere where
  {-# INLINE normalAt #-}
  distanceTo (Ray ro rd) (Sphere ce ra _) = if h < 0 then Nothing else closer
    where
      oc = ro - ce
      b = dot oc rd
      c = dot oc oc - ra * ra
      h = b * b - c
      near = -b - sqrt h
      far = -b + sqrt h
      closer = let val = min far near in if val < 0 then Nothing else Just val

  normalAt pt (Sphere ce _ _) = normalize $ pt - ce

data Plane = Plane
  { _planeOrigin    :: Point
  , _planeDirection :: Direction
  , _planeMaterial  :: Material
  }

instance HasMaterial Plane where
  {-# INLINE material #-}
  material = _planeMaterial

instance Primitive Plane where
  {-# INLINE normalAt #-}
  distanceTo (Ray ro rd) (Plane orig dir _) = if k < 0 then Nothing else Just k
    where
      k = - dot (ro - orig) dir / dot rd dir

  normalAt _ (Plane _ dir _) = normalize dir

-- |Existential qualifier for the 'Primitive' class.
-- Used to achieve dynamic dispatch known from OOP languages.
-- Makes representing the scene as a list of anything that implements the 'Primitive' class possible,
-- regardless of the actual data type underneath.
data Shape = forall a. Primitive a => Shape a

instance Primitive Shape where
  {-# INLINE distanceTo #-}
  {-# INLINE normalAt #-}
  {-# INLINE hit #-}
  distanceTo ray (Shape a) = distanceTo ray a
  normalAt p (Shape a) = normalAt p a
  hit ray f (Shape a) = hit ray f a

-- |A smart constructor for creating spheres.
-- Takes in the position, radius and material information associated with the sphere.
sphere :: Point -> Double -> Material -> Shape
sphere = Shape .:. Sphere

-- |A smart constructor upcasting the 'Plane' type to 'Shape'.
-- Takes in the origin point, "up" facing direction of the plane and material information.
plane :: Point -> Direction -> Material -> Shape
plane = Shape .:. Plane
