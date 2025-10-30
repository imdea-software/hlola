{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.JusterInputs where
import DecDyn
import Lola
import Syntax.HLPrelude
import Syntax.Booleans
import Data.Aeson
import GHC.Generics
import Data.Text (unpack)
import Data.Time

type TimeStamp = String
type EventId = Int
data Operation = NewEvent | Close | Bet | Withdraw | Other deriving (Show,Generic,Read,ToJSON,Eq)

instance FromJSON Operation where
  parseJSON (String text) = 
    case unpack text of
      "newEvent"       -> return NewEvent
      "close"          -> return Close
      -- "triggerForceMajeure"          -> return Close
      "bet"            -> return Bet
      "withdraw"       -> return Withdraw
      _                -> return Other

--- Query: https://api.tzkt.io/v1/operations/transactions?target=KT1D6XTy8oAHkUWdzuQrzySECCDMnANEchQq&select=diffs,parameter,id,timestamp'
-- Extract from path ".diffs[0].content.key"
eventIdNew :: Stream String
eventIdNew = Input "eventIdNew"

-- Extract from path ".parameter.value"
eventIdClose :: Stream String
eventIdClose = Input "eventIdClose"

-- Extract from path ".parameter.value.eventId"
eventIdWithdraw :: Stream String
eventIdWithdraw = Input "eventIdWithdraw"

eventId :: Stream EventId
eventId = "eventId" =: switch <$> Now operation <*> Now eventIdNew <*> Now eventIdClose <*> Now eventIdWithdraw
  where
  switch NewEvent x _ _ = read x
  switch Close    _ x _ = read x
  switch Withdraw _ _ x = read x
  switch _ _ _ _ = -1

-- Extract from path ".parameter.entrypoint"
operation :: Stream Operation
operation = Input "operation"

-- Extract from path ".id"
transId :: Stream Int
transId = Input "transId"

-- Extract from path ".timestamp"
timestamp :: Stream TimeStamp
timestamp = Input "timestamp"

time :: Stream UTCTime
time = "time" =:
  parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" <$> Now timestamp
