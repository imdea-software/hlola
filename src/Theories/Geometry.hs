{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Theories.Geometry where

import Syntax.HLPrelude
import Data.Aeson
import GHC.Generics
import qualified Prelude as P

data Point3 = P {x :: Double, y :: Double, z :: Double} deriving (Show,Generic,Read,FromJSON,ToJSON)
type Vector3 = Point3
type Polygon = [Point3]


crossProduct :: Vector3 -> Vector3 -> Vector3
crossProduct (P {x=ux, y=uy, z=uz}) (P {x=vx, y=vy, z=vz}) = P {
  x = uy*vz - uz*vy,
  y = uz*vx - ux*vz,
  z = ux*vy - uy*vx
  }

vecadd :: Vector3 -> Vector3 -> Vector3
vecadd (P {x=ux, y=uy, z=uz}) (P {x=vx, y=vy, z=vz}) = P {
  x = ux + vx,
  y = uy + vy,
  z = uz + vz
  }

projectToPlaneZ :: Vector3 -> Point3 -> Maybe Point3
projectToPlaneZ (P {x=ux, y=uy, z=uz}) (P {x=vx, y=vy, z=vz}) =
  if (vy == 0) then Nothing else
    let coef = - uy P./ vy in Just P {
        x = ux + coef*vx,
        y = 0,
        z = uz + coef*vz}

emptyPolygon :: Polygon
emptyPolygon = []

polyUnion :: Polygon -> Polygon -> Polygon
-- TODO: Implement correctly
polyUnion = (++)
