{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.NuSMVEx where

import Data.Aeson
import GHC.Generics
import Lola
import Syntax.HLPrelude
import DecDyn
import Lib.LTL
import Syntax.Booleans

data SenderState = Get | Send | WaitForAck deriving (Generic,Read,FromJSON,Eq)

spec :: Specification
spec = [out property]

senderState :: Stream SenderState
senderState = Input "senderState"

property :: Stream Bool
property = let
  senderWaitingAck = Now senderState === Leaf WaitForAck
  notSenderWaitingAck = "notSenderWaitingAck" =: Now senderState /== Leaf WaitForAck
  in
  "property" =: senderWaitingAck `implies` Now (yesterday $ historically $ notSenderWaitingAck)
