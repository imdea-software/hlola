module Lib.Pinescript where

import Lola
import Syntax.HLPrelude
import qualified Prelude as P
import Lib.Utils
import Syntax.Num
import Syntax.Booleans
import Syntax.Ord

sumLast :: (Num a, Streamable a) => Stream a -> Int -> Stream a
sumLast dec n = "sumLast" <: dec <: n =:
  sumLast dec n:@(-1,0) + Now dec - dec:@(-n,0)

sma :: (Fractional a, Streamable a) => Stream a -> Int -> Stream a
sma dec n = let
  name = "sma" <: dec <: n
  numerator = Now $ sumLast dec n
  denominator = fromIntegral <$> (min <$> Now instantN <*> fromIntegral n)
  in name =: numerator / denominator

wma :: (Fractional a, Streamable a) => Stream a -> Int -> Stream a
wma dec n = let
  name = "wma" <: dec <: n
  slots = min <$> Now instantN <*> Leaf n
  sumFirstN = fromIntegral <$> (div <$> slots * (slots + 1) <*> 2)
  body = foldl (+) 0 [dec :@ (-i,0) * Leaf (fromIntegral$n-i) | i<-[0..n-1]] / sumFirstN
  in
  name =: body

ema :: (Fractional a, Streamable a) => Stream a -> Int -> Stream a
ema dec n =
  let
    name = "ema" <: dec <: n
    emaprev = ema dec n :@ (-1,0)
    slots = fromIntegral <$> (min <$> Now instantN <*> Leaf n)
    multiplier = 2/slots
  in
    name =: (Now dec - emaprev) * multiplier + emaprev

macd :: (Fractional a, Streamable a) => Stream a -> Stream a
macd dec = "macd" <: dec =: Now (ema dec 12) - Now (ema dec 26)

macd_signal :: (Fractional a, Streamable a) => Stream a -> Stream a
macd_signal dec = "macd_signal" <: dec =: Now (ema (macd dec) 9)

crossover :: (Num a, Ord a, Streamable a) => Stream a -> Stream a -> Stream Bool
crossover d0 d1 = "crossover" <: d0 <: d1 =:
  Now d1 <= Now d0 && d0 :@(-1,0) <= d1 :@ (-1,0)
