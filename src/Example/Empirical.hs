{-# Language RebindableSyntax #-}
module Example.Empirical where

import Lola
import Syntax.HLPrelude
import Syntax.Booleans
import Syntax.Num()
import DecDyn

p :: Stream Bool
p = Input "p"

booleanPeriodWidth :: Int -> Stream Bool
booleanPeriodWidth n = "periodic_width" =: Now p === p :@(-n, Now p)

booleanPeriodHeight :: Int -> Stream Bool
booleanPeriodHeight n = "periodic_height" =: Now (carrier n) <*> Now p
 where
  carrier 0 = "carrier_period" <: 0 =: (==) <$> Now p
  carrier n = "carrier_period" <: n =: carrier (n-1) :@ (-1,Leaf (const True))

smoothPeriodWidth :: Int -> Stream Int
smoothPeriodWidth n = "smooth_period_width" =:
  if Now p === p :@ (-n, Now p) then 100 else
  if Now p === p :@ (-n-1, Now p) || Now p === p :@ (-n+1, Now p) then 50 else
  if Now p === p :@ (-n-2, Now p) || Now p === p :@ (-n+2, Now p) then 25 else
  0

smoothPeriodHeight :: Int -> Stream Int
smoothPeriodHeight n = "smooth_period_height" =:
  if Now (carrier n) <*> Now p then 100 else
  if Now (carrier (n-1)) <*> Now p || Now (carrier (n+1)) <*> Now p then 50 else
  if Now (carrier (n-2)) <*> Now p || Now (carrier (n+2)) <*> Now p then 25 else
  0
  where
    carrier 0 = "carrier_smooth_period" <: 0 =: (==) <$> Now p
    carrier n = "carrier_smooth_period" <: n =: carrier (n-1) :@ (-1,Leaf (const True))

windowTrueWidth :: Int -> Stream Int
windowTrueWidth n = "windowTrueWidth" =: let
  add = if Now p then 1 else 0
  rmv = if p :@ (-n, Leaf False) then 1 else 0
  in add - rmv

windowTrueHeight :: Int -> Stream Int
windowTrueHeight n = "windowTrueHeight" =: let
  add = if Now p then 1 else 0
  rmv = if Now (carrier n) then 1 else 0
  in add - rmv
  where
    carrier 0 = "carrier_window" <: 0 =: Now p
    carrier n = "carrier_window" <: n =: carrier (n-1) :@ (-1,Leaf False)

data TestType = PeriodWidth | PeriodHeight | SmoothPeriodWidth | SmoothPeriodHeight | WindowTrueWidth | WindowTrueHeight deriving Read

getSpec :: (TestType, Int) -> Specification
getSpec (tt, n) = out p:getStreams tt n
  where
  getStreams PeriodWidth n = [out $ booleanPeriodWidth n]
  getStreams PeriodHeight n = [out $ booleanPeriodHeight n]
  getStreams SmoothPeriodWidth n = [out $ smoothPeriodWidth n]
  getStreams SmoothPeriodHeight n = [out $ smoothPeriodHeight n]
  getStreams WindowTrueWidth n = [out $ windowTrueWidth n]
  getStreams WindowTrueHeight n = [out $ windowTrueHeight n]
  -- getStreams Since2 n = [out $ since2 n p]
  -- getStreams QSince2 n = [out $ qsince2 n p]
  -- getStreams Since12 n = [out $ since n p, out $ since2 n p]
  -- getStreams QSince12 n = [out $ qsince2 n p, out $ qsince2 n p]
