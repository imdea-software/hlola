{-# LANGUAGE RebindableSyntax  #-}
module Lib.MTL where

import Lola
import Syntax.HLPrelude
import Syntax.Booleans
import Syntax.Ord
import Syntax.Num
import Lib.Utils

-- Reference: https://www.cs.ox.ac.uk/people/james.worrell/mtlsurvey08.pdf

-- phi U_{a,b} psi
untilMTL :: (Int, Int) -> Stream Bool -> Stream Bool -> Stream Bool
untilMTL (a, b) phi psi = let
    name = "until_(" ++ show a ++ "," ++ show b ++ ")" <: phi <: psi
  in name =: until' a b phi psi

until' a b phi psi
  | a == b = psi:@(b,False)
  | otherwise = psi:@(a,False) || (phi:@(a, True) && until' (a+1) b phi psi)

notMTL :: Stream Bool -> Stream Bool
notMTL dec = "not" <: dec =: not (Now dec)

andMTL :: Stream Bool -> Stream Bool -> Stream Bool
andMTL d0 d1 = "and" <: d0 <: d1 =: Now d0 && Now d1

-- Phi holds at some point between a and b
eventuallyMTL :: (Int, Int) -> Stream Bool -> Stream Bool
eventuallyMTL (a,b) phi = let
    name = "eventually_(" ++ show a ++ "," ++ show b ++ ")" <: phi
  in name =: foldl (||) (Leaf False) [phi :@ (i, False) | i <- [a..b]]

historicallyMTL :: Int -> Stream Bool -> Stream Bool
historicallyMTL k dec = "historicallyMTL" <: k <: dec =:
  Now (consecutiveTrueMTL dec) >= Leaf k

consecutiveTrueMTL :: Stream Bool -> Stream Int
consecutiveTrueMTL dec = "consecutiveTrueMTL" <: dec =:
  if not $ Now dec then 0 else consecutiveTrueMTL dec :@ (-1, 0) + 1
