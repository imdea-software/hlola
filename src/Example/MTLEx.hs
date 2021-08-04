{-# LANGUAGE RebindableSyntax  #-}
module Example.MTLEx where

import Lola
import Syntax.HLPrelude
import DecDyn
import Lib.MTL
import Syntax.Booleans

spec :: Specification
spec = [out property]

alarm :: Stream Bool
alarm = Input "alarm"

allclear :: Stream Bool
allclear = Input "allclear"

shutdown :: Stream Bool
shutdown = Input "shutdown"

property :: Stream Bool
property = let
  willClear = eventually (0,10) allclear
  willShutdown = eventually (10,10) shutdown
  in
  "property" =: Now alarm `implies` Now willClear || Now willShutdown
