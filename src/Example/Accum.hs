{-# Language RebindableSyntax #-}
module Example.Accum where

import Lola
import Syntax.HLPrelude
import Syntax.Booleans
import Syntax.Num()
import DecDyn

spec = [out s]

p :: Stream Int
p = Input "p"

s :: Stream Int
s = "s" =: s :@ (-1,0) + Now p
