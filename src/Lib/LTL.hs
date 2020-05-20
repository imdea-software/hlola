module Lib.LTL where

import Lola
import Syntax.HLPrelude
import Lib.Utils
import Syntax.Num
import Syntax.Booleans
import qualified Prelude as P ((&&), (||))

-- Reference:
-- http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.107.788&rep=rep1&type=pdf

historically :: Stream Bool -> Stream Bool
--historically p = "historically" <: p =: Now p && historically p :@(-1,True)
historically = hFoldl "historically" (P.&&) True

once :: Stream Bool -> Stream Bool
--once p = "once" <: p =: Now p || once p :@(-1,False)
once = hFoldl "once" (P.||) False

since :: Stream Bool -> Stream Bool -> Stream Bool
since p q = "since" <: p <: q =: Now q || ( Now p && p `since` q :@(-1, Leaf False))

yesterday :: Stream Bool -> Stream Bool
yesterday p = "yesterday" <: p =: p:@(-1, Leaf False)

zesterday :: Stream Bool -> Stream Bool
zesterday p = "zesterday" <: p =: p:@(-1, Leaf True)

-- Quantitative:
nViolations :: Stream Bool -> Stream Int
nViolations = hFoldl "nViolations" (\n b -> n+if b then 0 else 1) 0

percentViolations :: Stream Bool -> Stream Double
percentViolations dec = "percViolations" <: dec =: Now (nViolations dec) `intdivide` Now instantN
