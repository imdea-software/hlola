{-# LANGUAGE RebindableSyntax  #-}
module Example.SliceEx where

import Lola
import Syntax.HLPrelude
import Syntax.Num
import Syntax.Ord
import DecDyn
import Prelude ((>), (<))

spec :: Specification
spec = [out clockodd]

n :: Stream Int
n = Input "n"

clock :: Stream Int
clock = "clock" =: clock :@ (-1,0) + 1

clockodd :: Stream Bool
clockodd = "clocks" =: any (Prelude.>3) <$> clock :@@ Now n
