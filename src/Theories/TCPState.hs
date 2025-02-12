{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Theories.TCPState where
import Data.Aeson
import GHC.Generics
import Data.Default

-- State example theory
data PacketType = SYN | SYNACK | ACK
  deriving (Generic,Show,Read,FromJSON,Eq,ToJSON,Ord)
data TCPState = Uninit | TCPError | SYNED | SYNACKED
  deriving (Generic,Show,ToJSON,Eq,Read,FromJSON,Ord)
data SourceDest = SD String String
  deriving (Generic,Show)

instance Default PacketType where
  def = SYN

instance Default TCPState where
  def = Uninit

getNextState :: TCPState -> PacketType -> TCPState
getNextState Uninit SYN = SYNED
getNextState SYNED SYNACK = SYNACKED
getNextState SYNACKED ACK = Uninit
getNextState _ _ = TCPError
