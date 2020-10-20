{-# LANGUAGE RebindableSyntax  #-}
module Lib.MTL where

import Lola
import Syntax.HLPrelude
import Syntax.Booleans
import Syntax.Ord
import Syntax.Num()

-- Reference: https://www.cs.ox.ac.uk/people/james.worrell/mtlsurvey08.pdf

-- phi U_{a,b} psi
until :: (Int, Int) -> Stream Bool -> Stream Bool -> Stream Bool
until (a, b) phi psi = let
    name = "until_(" ++ show a ++ "," ++ show b ++ ")" <: phi <: psi
  in name =: until' a
  where
  until' a
    | a == b = psi:@(b,Leaf False)
    | otherwise = psi:@(a,Leaf False) || (phi:@(a, Leaf True) && until' (a+1))

notStrm :: Stream Bool -> Stream Bool
notStrm dec = "not" <: dec =: not (Now dec)

and :: Stream Bool -> Stream Bool -> Stream Bool
and d0 d1 = "and" <: d0 <: d1 =: Now d0 && Now d1

-- Phi holds at some point between a and b
eventually :: (Int, Int) -> Stream Bool -> Stream Bool
eventually (a,b) phi = let
    name = "eventually_(" ++ show a ++ "," ++ show b ++ ")" <: phi
  in name =: foldl (||) (Leaf False) [phi :@ (i,Leaf False) | i <- [a..b]]

historically :: Int -> Stream Bool -> Stream Bool
historically k dec = "historically" <: k <: dec =:
  Now (consecutiveTrue dec) >= Leaf k

consecutiveTrue :: Stream Bool -> Stream Int
consecutiveTrue dec = "consecutiveTrue" <: dec =:
  if not $ Now dec then 0 else consecutiveTrue dec :@ (-1, 0) + 1
