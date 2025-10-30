{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Theories.Geometry2D where
-- (Point2(..), Vector2(..), angle_between_vectors, plus, norm, distance, minus, dot, cross, intersectionDistance, Polygon, pointInPoly, polygonSides, distancePointSegment, pointbetween) where

import Data.Aeson
import Data.Tuple
import Data.Maybe
import Data.List
import Data.Function
import GHC.Generics
-- import Debug.Trace

data Point2 = P {x :: Double, y :: Double} deriving (Show,Generic,Read,FromJSON,ToJSON,Eq)
type Vector2 = Point2

type Polygon = [Point2]
data Line = Sloped {lineSlope, lineYIntercept :: Double} |
            Vert {lineXIntercept :: Double} deriving (Eq, Show, Generic, FromJSON, ToJSON, Read)

-- data LineDir = Pos | Neg deriving (Show, Eq)

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

pointbetween :: Point2 -> Point2 -> Point2 -> Bool
pointbetween (P px py) (P p0x p0y) (P p1x p1y) = between px p0x p1x && between py p0y p1y

data Side = LS | RS | ZS deriving (Show, Eq)

pointside :: Line -> Point2 -> Side
pointside l p
  | inline p l = ZS
  | intersects p l = LS
  | otherwise = RS
  where
  inline (P px _) (Vert xint)  = px `approx` xint
  inline  (P px py) (Sloped m b) = py `approx` (m * px + b)
  approx x y = abs (x-y) < 0.00001

perpLine :: Line -> Point2 -> Line
perpLine (Vert _) (P _ py) = Sloped 0 py
perpLine (Sloped 0 _) (P px _) = Vert px
perpLine (Sloped m _) (P px py) = Sloped m' (py-m'*px)
  where m' = tan (atan m + pi/2)

sweepPerp :: Point2 -> Polygon -> Line -> Maybe ((Point2, Point2), Point2)
sweepPerp uavloc poly wind = let
  perpl = perpLine wind
  edges = filter (isdivider perpl poly) poly
  lines = concatMap (around 10 perpl) edges
  -- vectors = map line2points lines
  sides = polygonSides poly
  crosses = [[fmap (\x->(x,originp)) $ lineCrossSide l s | s <- sides] | (l, originp) <- lines]
  inters = map (\([(x,op),(y,_)]) -> ((x,y),op)) $ filter ((==2).length) (map catMaybes crosses)
  interss = inters ++ map (\((x,y),op) -> ((y,x),op)) inters
  (nearest,op) = head (sortBy (compare `on` (distance uavloc.fst.fst)) interss)
  in if inters == [] then Nothing else Just (nearest, op)

lineCrossSide :: Line -> (Point2, Point2) -> Maybe Point2
lineCrossSide l s@(p0, p1) = let
  ls = carrier s
  inters = lineCross l ls
  in inters >>= (\x->if pointbetween x p0 p1 then Just x else Nothing)

lineCross :: Line -> Line -> Maybe Point2
lineCross (Vert _) (Vert _) = Nothing
lineCross (Sloped m y) (Vert x) = Just (P x (m*x+y))
lineCross (Vert x) (Sloped m y) = Just (P x (m*x+y))
lineCross (Sloped m0 y0) (Sloped m1 y1)
  | m0 == m1 = Nothing
  | otherwise = let
    x = (y1 - y0) / (m0-m1)
    in Just (P x (m0*x + y0))

-- line2points :: Line -> (Point2, Vector2)
-- line2points (Vert x) = ((P x 0), (P x 1))
-- line2points (Sloped m y) = ((P 0 y), (P 1 (m+y)))

around :: Double -> (Point2 -> Line) -> Point2 -> [(Line, Point2)]
around d specializer p = let
  line = specializer p
  in [(moveline d line, p), (moveline (-d) line, p)]

moveline :: Double -> Line -> Line
moveline d (Vert x) = Vert (d+x)
moveline d (Sloped m y) = let
  diffy = d / cos (atan m)
  in Sloped m (y+diffy)

isdivider :: (Point2 -> Line) -> Polygon -> Point2 -> Bool
isdivider specializer poly p = let
  line = specializer p
  sides = map (pointside line) poly
  nubfilteredsides = nub (filter (/=ZS) sides)
  in length nubfilteredsides == 1

slicer :: Polygon -> ((Point2, Point2), Point2) -> Polygon
slicer poly (trip, originpoint) = let
  parts = partition (comparewithline (carrier trip)) poly
  newpolypoints = keepPart originpoint parts
  newpoly = joinwith trip newpolypoints poly
  in newpoly
  where
  keepPart p (x,y) = if elem p x then y else x

comparewithline :: Line -> Point2 -> Bool
comparewithline l p = pointside l p == RS

joinwith :: (Point2, Point2) -> [Point2] -> [Point2] -> [Point2]
joinwith x y z = (jw' [] x y z)
  where
    jw' l (t1,t2) [] _ = let
      tryp1 = t1:t2:l
      tryp2 = t2:t1:l
      in if validpoly tryp1 then tryp1 else if validpoly tryp2 then tryp2 else error "invalid1"
    jw' l trip@(t1,t2) (np:npr) (p:pr)
      | p /= np = let
        tryp1 = l ++ (t1:t2:np:npr)
        tryp2 = l ++ (t2:t1:np:npr)
        in if validpoly tryp1 then tryp1 else if validpoly tryp2 then tryp2 else error "invalid2"
      | otherwise = jw' (l++[np]) trip npr pr

validpoly :: [Point2] -> Bool
validpoly poly = let
  sides = polygonSides poly
  intersections = [s1a==s2a||s1a==s2b||s1b==s2a||s1b==s2b || check s1 s2 | s1@(s1a,s1b) <- sides, s2@(s2a,s2b) <- sides]
  check s1 (s2a,s2b) = let
    ret = pointside (carrier s1) s2a == pointside (carrier s1) s2b
    in ret
  in and intersections


-- polygonSides :: Polygon -> [(Point2, Point2)]
-- carrier :: (Point2, Point2) -> Line
-- lineCross :: Line -> Line -> Maybe Point2


getangle :: (Point2, Point2) -> Double
getangle (P x0 y0,P x1 y1) = let
  p = (x1-x0, y1-y0)
  in angle p
  where
  angle (0,y) = if y>0 then pi/2 else -pi/2
  angle (x,y) = atan (x/y)
