{-# LANGUAGE RebindableSyntax  #-}
module Example.DynMTLEx where

import Lola
import Syntax.HLPrelude
import DecDyn
import Lib.DynMTL
import Syntax.Booleans

spec :: Specification
spec = [out property]

alarm :: Stream Bool
alarm = Input "alarm"

allclear :: Stream Bool
allclear = Input "allclear"

n :: Stream Int
n = Input "n"

property :: Stream Bool
property = until (0,Now n) alarm allclear
