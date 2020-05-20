{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Theories.Geometry2D where

import Data.Aeson
import GHC.Generics

data Point2 = P {x :: Double, y :: Double} deriving (Show,Generic,Read,FromJSON,ToJSON)
type Vector2 = Point2

data DebugGeo = DG {
  uavVector :: Double, targVector :: Double,
  uavInters :: Point2, targInters :: Point2,
  dist :: Double} deriving (Show, Generic, ToJSON)

plus :: Vector2 -> Vector2 -> Vector2
P x0 y0 `plus` P x1 y1 = P (x0 + x1) (y0 + y1)

minus :: Vector2 -> Vector2 -> Vector2
P x0 y0 `minus` P x1 y1 = P (x0 - x1) (y0 - y1)

crossPoint :: (Point2, Vector2) -> (Point2, Vector2) -> Either String DebugGeo
-- Origin, target
crossPoint x y = crossPoint' x y False

crossPoint' :: (Point2, Vector2) -> (Point2, Vector2) -> Bool -> Either String DebugGeo
crossPoint' (_, P 0 _) (_, P 0 _) _ = Left "Parallel vectors with x=0"
crossPoint' r0@(_, P 0 _) r1 _ = crossPoint' r1 r0 True
crossPoint' (p0@(P p0a p0b), P v0a v0b) (p1@(P p1a p1b), P v1a v1b) flippedArgs =
  let
    denom = v0b * (v1a/v0a) - v1b
    numer = (p1b-p0b) - (p1a-p0a) * (v0b/v0a)
    x1 = numer/denom
    x0 = (p1a + x1*v1a - p0a) / v0a
    ip0 = P (p0a + x0 * v0a) (p0b + x0 * v0b)
    ip1 = P (p1a + x1 * v1a) (p1b + x1 * v1b)
    (uavPoint,uavVector,uavIntersPoint,targPoint,targVector,targIntersPoint) = if flippedArgs then (p1,x1,ip1,p0,x0,ip0) else (p0,x0,ip0,p1,x1,ip1)
    dist = distance uavIntersPoint targPoint
  in
    if denom == 0 then Left "Parallel vectors" else
    (if uavVector < 0 then Left "Wrong direction" else
    Right $ DG uavVector targVector uavIntersPoint targIntersPoint dist)

distance :: Point2 -> Point2 -> Double
distance (P x0 y0) (P x1 y1) = sqrt $ ((x0-x1)^2) + ((y0-y1)^2)

norm :: Vector2 -> Double
norm v = distance (P 0 0) v

intersectionDistance :: (Point2, Vector2) -> (Point2, Vector2) -> Either String DebugGeo
intersectionDistance = crossPoint

dot :: Vector2 -> Vector2 -> Double
dot (P x0 y0) (P x1 y1) = x0*x1 + y0*y1

cross :: Vector2 -> Vector2 -> Double
cross (P x0 y0) (P x1 y1) = x0*y1 - y0*x1

angle_between_vectors :: Vector2 -> Vector2 -> Double
angle_between_vectors v1 v2 = let
  cosang = dot v1 v2
  sinang = cross v1 v2
  in atan2 sinang cosang

scalarmul :: Vector2 -> Double -> Vector2
scalarmul (P x y) d = P (x*d) (y*d)
