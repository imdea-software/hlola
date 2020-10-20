{-# LANGUAGE RebindableSyntax  #-}
module Lib.QMTL where

import Lola
import Syntax.HLPrelude
import Lib.Utils
import Syntax.Booleans
import Syntax.Ord
import Syntax.Num
import qualified Prelude as P
import Data.Maybe

medianLast :: (Eq a,Fractional a, Streamable a) => Int -> Stream a -> Stream a
medianLast k dec = "medianLast" <: k <: getId dec =: let
  denom = strMap ("min" <: k) (min k) instantN
  numerator = foldl (+) (Leaf 0) [dec :@ (i, 0) | i <- [(1-k)..0]]
  in numerator / (fromIntegral <$> Now denom)

-- Reference: https://www.cs.ox.ac.uk/people/james.worrell/mtlsurvey08.pdf

-- -- phi U_{a,b} psi
-- untilMTL :: (Int, Int) -> Stream Bool -> Stream Bool -> Stream Bool
-- untilMTL (a, b) phi psi = "until_(" `T.append` (fromString$show a ++ [','] ++ show b ++ [')']) <: getId phi <: getId psi =:
--   let
--     minPsi = foldl (\acci (b,i) -> if b then i else acci) (Leaf (b+1)) [(psi@:(a+b-i,False),Leaf (a+b-i)) | i <- [a..b]]
--     minNotPhi = foldl (\acci (b,i) -> if not b then i else acci) (Leaf (b+1)) [(phi@:(a+b-i,True),Leaf (a+b-i)) | i <- [a..b]]
--   in minPsi <= minNotPhi && minPsi <= Leaf b

-- notMTL :: Stream Bool -> Stream Bool
-- notMTL dec = "not" <: getId dec =: not (Now dec)

-- andMTL :: Stream Bool -> Stream Bool -> Stream Bool
-- andMTL d0 d1 = "and" <: getId d0 <: getId d1 =: Now d0 && Now d1

-- historicallyMTL :: Int -> Stream Bool -> Stream Bool
-- historicallyMTL k dec = "historicallyMTL" <: (fromString.show) k <: getId dec =:
--   Now (consecutiveTrueMTL dec) >= Leaf k

-- consecutiveTrueMTL :: Stream Bool -> Stream Int
-- consecutiveTrueMTL dec = "consecutiveTrueMTL" <: getId dec =:
--   if not $ Now dec then 0 else consecutiveTrueMTL dec @: (-1, 0) + 1
