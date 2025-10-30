module Theories.Discretizer (updatefield, defaultField, Field) where
import Theories.Geometry2D

type Field = [Point2]

defaultField = [P i j | i <- [0,50..width], j <- [0,50..height]]

width = 511
height = 511

updatefield :: Field -> Point2 -> Int -> Maybe Field
updatefield oldfield target seed = let
  radius = [-20,-18..20]
  newps = [ret | offset <- [P i j | i <- radius, j <- radius], let ret = target `plus` offset, valid ret]
  representatives = filter valid $ map (plus$getoff seed) defaultField
  in Just $ representatives ++ newps

valid ret = pointbetween ret (P 0 0) (P width height)

getoff :: Int -> Point2
getoff 1 = P 25 25
getoff 2 = P 12 25
getoff 3 = P 25 12
getoff 4 = P 25 37
getoff 5 = P 37 25
getoff 6 = P 12 12
getoff 7 = P 12 37
getoff 8 = P 37 12
getoff 9 = P 37 37
getoff _ = P 0 0
