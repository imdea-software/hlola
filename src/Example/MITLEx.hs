{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.MITLEx where

import Data.Aeson
import GHC.Generics
import Lola
import Syntax.HLPrelude
import DecDyn
import Lib.MITL
import Syntax.Booleans

data Event
    = Alarm
    | AllClear
    | Shutdown
  deriving (Generic,Read,FromJSON,Eq)

spec :: Specification
spec = [out property]

event :: Stream Event
event = Input "event"

property :: Stream Bool
property = let
    now x = Now event === Leaf x
    allClear = "allClear" =: now AllClear
    shutdown = "shutdown" =: now Shutdown
    body = now Alarm `implies` (Now (eventuallyMITL (0,10) allClear) || Now (eventuallyMITL (10,10) shutdown))
  in
    "property" =: body

untilEx :: Specification
untilEx = [out p, out q, out theuntil]
 where
  -- Input variables
  p = Input "p"
  q = Input "q"
  -- Outputs variables
  theuntil = untilMITL (-1,1) p q
