{-# LANGUAGE RebindableSyntax  #-}
module Example.PinescriptEx where

-- Lola imported as a library
import           Lola
import           Syntax.HLPrelude
import           Syntax.Booleans
import           Syntax.Ord
import           Syntax.Num
import Lib.Pinescript
import DecDyn

pinescriptEx :: [Stream Double]
pinescriptEx = [positionsize]
 where
   buy_size = Leaf 10
   sell_size = Leaf 10
   close = Input "close" :: Stream Double
   high = Input "high" :: Stream Double
   ema18 = "ema18" =: Now (ema close 18)
   macd_sell_sig = crossover (macd_signal close) (macd close)
   macd_buy_sig = crossover (macd close) (macd_signal close)
   buy = "buy" =: Now ema18 < Now high && Now macd_buy_sig
   sell = "sell" =: not (macd_buy_sig :@ (-1,False)) && Now macd_sell_sig && (positionsize:@(-1,0) > Leaf 0)
   positionsize = "positionsize" =: positionsize :@ (-1,0) +
    if Now buy then buy_size else Leaf 0 +
    if Now sell then sell_size else Leaf 0

wmaUsage :: [Stream Double]
wmaUsage = [ou]
 where
   close = Input "close" :: Stream Double
   ou = wma close 10

buy_size = 10
sell_size = 10

close = Input "close" :: Stream Double
high = Input "high" :: Stream Double

ema18 = ema close 18
signalLine = macd_signal close
macdLine = macd close
macd_sell_sig = crossover signalLine macdLine
macd_buy_sig = crossover macdLine signalLine

buy = "buy" =: Now ema18 < Now high && Now macd_buy_sig
sell = "sell" =: not (macd_buy_sig :@ (-1,False)) && Now macd_sell_sig && positionsize :@ (-1,0) > 0

positionsize :: Stream Double
positionsize = "positionsize" =:
  positionsize :@ (-1,0) +
  (if Now buy then buy_size else 0) -
  (if Now sell then sell_size else 0)

spec :: Specification
spec = [out positionsize]
