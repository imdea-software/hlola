{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.AndoniEx where

import Lola
import Syntax.HLPrelude
import DecDyn
import Syntax.Booleans
import Syntax.Num
import Syntax.Ord
import Data.List
import qualified Prelude as P

spec :: Specification
spec = [out hasoutlier, out y]

t :: Double
t = 4

sensors :: Stream [Double]
sensors = Input "sensors"

x :: Stream Double
x = Input "x"

avgsensor :: Stream Double
avgsensor = "avgsensor" =:
  avg <$> Now sensors
  where 
  avg l = sum l P./ genericLength l

hasoutlier :: Stream Bool
hasoutlier = "hasoutlier" =: let
  in check <$> Now avgsensor <*> Now sensors
  where 
  check avga = any (\ai -> abs (ai - avga) P.> t)

bigx :: Stream Bool
bigx = "bigx" =: Now x > Leaf t

y :: Stream Double
y = "y" =: let
  ygtt = hasoutlier :@ (-1, Leaf False) -- ygtt represents the requirement that y has to be greater than t
  yltx = not (Now hasoutlier) && Now x > Leaf t -- yltx represents that y is reuqired to be lower than x
  in
  if ygtt && yltx
  then (Leaf t+Now x)/2 -- If we were using Integers, we may not find a number inbetween t and x
  else if ygtt then Leaf t+1
  else if yltx then Now x-1
  else Now x
