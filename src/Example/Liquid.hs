{-@ LIQUID "--no-termination" @-}
module Example.Liquid where

import           DecDyn
import           Syntax.Num
import           Lola

{-@ sumLast :: Stream Int -> Nat -> Stream Int @-}
sumLast :: Stream Int -> Int -> Stream Int
sumLast dec n = "sumLast" <: dec <: n =:
  (sumLast dec n):@(-1,0)
  + Now dec - dec:@(-n,0)

r :: Stream Int
r = Input "r"

s :: Stream Int
s = sumLast r 5

-- Rejected by Liquid Haskell.
s' :: Stream Int
s' = sumLast r (-1)

{-
Targets: Liquid.hs

**** [Checking: Liquid.hs] *****************************************************

**** DONE:  A-Normalization ****************************************************
 

**** DONE:  Extracted Core using GHC *******************************************
 

**** DONE:  Transformed Core ***************************************************
 
Working 175% [=================================================================================
=================================]

**** DONE:  annotate ***********************************************************
 

**** RESULT: UNSAFE ************************************************************


 /home/martin/HLola/src/Example/Liquid.hs:22:16-19: Error: Liquid Type Mismatch
  
 22 | s' = sumLast r (-1)
                     ^^^^
  
   Inferred type
     VV : {v : GHC.Types.Int | v == (-1)}
  
   not a subtype of Required type
     VV : {VV : GHC.Types.Int | VV >= 0}
-}
