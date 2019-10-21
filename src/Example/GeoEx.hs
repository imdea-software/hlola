{-# LANGUAGE RebindableSyntax  #-}
module Example.GeoEx where

-- Lola imported as a library
import Lola
import Syntax.HLPrelude
import Theories.Geometry --(Point2(..))
import Data.Maybe
import DecDyn

projectToFloor :: Point3 -> Vector3 -> Vector3 -> Polygon
projectToFloor pos up right = let
  perpVector = crossProduct up right
  point1 = pos
  point2 = pos `vecadd` up
  point3 = pos `vecadd` right
  point4 = pos `vecadd` up `vecadd` right in
  mapMaybe (projectToPlaneZ perpVector) [point1, point2, point3, point4]

geoEx :: Specification
geoEx = [out coveredSurface]
  where
   position = Input "position" :: Stream Point3
   up = Input "up" :: Stream Vector3
   right = Input "right" :: Stream Vector3
   coveredPoly = "coveredPoly" =: projectToFloor <$> Now position <*> Now up <*> Now right
   coveredSurface = "coveredSurface" =: polyUnion <$> coveredSurface :@ (-1, emptyPolygon) <*> Now coveredPoly
