{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Theories.Geometry2D (Point2(..), Vector2(..), angle_between_vectors, plus, norm, distance, minus, dot, cross, intersectionDistance, Polygon, pointInPoly, polygonSides, distancePointSegment) where

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

intersectionDistance :: (Point2, Vector2) -> (Point2, Vector2) -> Either String Double
intersectionDistance x y = crossPoint x y >>= (Right).dist

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

-- From http://mathworld.wolfram.com/ComplexDivision.html
complexdiv :: Point2 -> Point2 -> Point2
complexdiv (P a b) (P c d) = P ((a*c+b*d) / (c*c+d*d)) ((b*c-a*d) / (c*c+d*d))

-- From https://demonstrations.wolfram.com/DistanceOfAPointToASegment:
-- To compute the distance from point p to segment ab (all as complex numbers)
-- compute first z=(p-a)/(b-a). If 0 ≤ Re[z] ≤ 1 then the distance is equal to
-- Abs[Im[z](b-a)]. If not, it is equal to the smallest of the distances from p
-- to a or to b.
distancePointSegment :: Point2 -> (Point2, Point2) -> Double
distancePointSegment p (a,b)
  | 0 <= rez && rez <= 1 = norm $ bminusa `scalarmul` imz
  | otherwise = min (distance p a) (distance p b)
  where
  bminusa = b `minus` a
  P rez imz = (p `minus` a) `complexdiv` bminusa


-- Taken from Rosetta Code and tested with Codewars kata
-- https://rosettacode.org/wiki/Ray-casting_algorithm#Haskell
-- https://www.codewars.com/kata/530265044b7e23379d00076a/train/haskell

type Polygon = [Point2]
data Line = Sloped {lineSlope, lineYIntercept :: Double} |
            Vert {lineXIntercept :: Double}

polygonSides :: Polygon -> [(Point2, Point2)]
polygonSides poly@(p1 : ps) = zip poly $ ps ++ [p1]

intersects :: Point2 -> Line -> Bool
{- @intersects (px, py) l@ is true if the ray {(x, py) | x ≥ px}
intersects l. -}
intersects (P px _)  (Vert xint)  = px <= xint
intersects (P px py) (Sloped m b) | m < 0     = py <= m * px + b
                                 | otherwise = py >= m * px + b

onLine :: Point2 -> Line -> Bool
{- Is the point on the line? -}
onLine (P px _)  (Vert xint)  = px == xint
onLine (P px py) (Sloped m b) = py == m * px + b

carrier :: (Point2, Point2) -> Line
{- Finds the line containing the given line segment. -}
carrier ((P ax ay), (P bx by)) | ax == bx  = Vert ax
                             | otherwise = Sloped slope yint
  where slope = (ay - by) / (ax - bx)
        yint = ay - slope * ax

between :: Ord a => a -> a -> a -> Bool
between x a b | a > b     = b <= x && x <= a
              | otherwise = a <= x && x <= b

pointInPoly :: Point2 -> Polygon -> Bool
pointInPoly p@(P px py) = f 0 . polygonSides
  where f n []                             = odd n
        f n (side : sides) | far           = f n       sides
                           | onSegment     = True
                           | rayIntersects = f (n + 1) sides
                           | otherwise     = f n       sides
          where far = not $ between py ay by
                onSegment | ay == by  = between px ax bx
                          | otherwise = p `onLine` line
                rayIntersects =
                    intersects p line &&
                    (py /= ay || by < py) &&
                    (py /= by || ay < py)
                ((P ax ay), (P bx by)) = side
                line = carrier side
