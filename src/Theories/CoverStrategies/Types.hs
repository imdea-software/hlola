module Theories.CoverStrategies.Types where
import Data.Array

type Position = (Int, Int)

type Matrix = Array Position Int

dfltmatrix :: Matrix
dfltmatrix = array ((0,0), (-1,-1)) []

creatematrix :: (Position, String) -> Matrix
creatematrix ((w,h),s) = listArray ((0,0),(w-1,h-1)) (map initval s)
  where
  initval '0' = -1
  initval _ = 0
