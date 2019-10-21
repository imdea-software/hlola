{-# LANGUAGE RebindableSyntax  #-}
module Lib.MITL where

import Lola
import Syntax.HLPrelude
import Syntax.Booleans
import Syntax.Ord
import Lib.Utils

-- Reference: https://www.cs.ox.ac.uk/people/james.worrell/mtlsurvey08.pdf

-- phi U_{a,b} psi
untilMITL :: (Int, Int) -> Stream Bool -> Stream Bool -> Stream Bool
untilMITL (a, b) phi psi = let
    name = "until_(" ++ show a ++ "," ++ show b ++ ")" <: phi <: psi
  in name =: until' a b phi psi

until' a b phi psi
  | a == b = psi:@(b,False)
  | otherwise = psi:@(a,True) && (phi:@(a, False) || until' (a+1) b phi psi)

notMITL :: Stream Bool -> Stream Bool
notMITL dec = "not" <: dec =: not (Now dec)

andMITL :: Stream Bool -> Stream Bool -> Stream Bool
andMITL d0 d1 = "and" <: d0 <: d1 =: Now d0 && Now d1

-- Phi holds at some point between a and b
eventuallyMITL :: (Int, Int) -> Stream Bool -> Stream Bool
eventuallyMITL (a,b) phi = let
    name = "eventually_(" ++ show a ++ "," ++ show b ++ ")" <: phi
  in name =: foldl (||) (Leaf False) [phi :@ (i, False) | i <- [a..b]]
