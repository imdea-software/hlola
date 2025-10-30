{-#LANGUAGE ScopedTypeVariables#-}
{-#LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Interpreter.JusterTheory where
import Interpreter.Theory
import Data.Default
import Data.Proxy
import Control.Unification
import Data.Constraint (Dict(..), (\\) )
import qualified Lib.DynPar as DP (over)
import Data.Typeable
import Data.Aeson
import GHC.Generics
import Data.Text (unpack)
import Data.Time
import qualified Data.Map.Strict as Map

data Operation = NewEvent | Close | Bet | Withdraw | Other deriving (Show,Generic,Read,ToJSON,Eq,Ord)

instance FromJSON Operation where
  parseJSON (String text) = 
    case unpack text of
      "newEvent"       -> return NewEvent
      "close"          -> return Close
      "triggerForceMajeure" -> return Close
      "bet"            -> return Bet
      "withdraw"       -> return Withdraw
      _                -> return Other

instance Default Operation where
  def = Other

data State = Idle | Fault | Open | Closed deriving (Show,Generic,Read,FromJSON,ToJSON,Eq,Ord)

instance Default State where
  def = Idle

instance Default UTCTime where
  def = parseTS "2000-01-01T00:00:00Z"

parseTS :: String -> UTCTime
parseTS = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

checkdiffs :: UTCTime -> Map.Map Int UTCTime -> Bool
checkdiffs ts = all ((<240*60*60).floor.diffUTCTime ts).Map.elems

makediff :: UTCTime -> Map.Map Int UTCTime -> Map.Map Int Integer
makediff ts m = Map.map (\tt -> floor$diffUTCTime ts tt) m

typesTB :: TheoryBuilder
typesTB =
  typeSynonym "EventId" (Proxy @Int)
  >> addType (Proxy @Operation)
  >> addType (Proxy @State)
  >> addType (Proxy @UTCTime)
  >> addType (Proxy @Integer)

funsTB :: TheoryBuilder
funsTB = do
  addSymbol "parseTS" $ totheory parseTS
  addSymbol "checkdiffs" $ totheory checkdiffs
  addSymbol "makediff" $ totheory makediff

justertheory :: TheoryBuilder
justertheory = 
  typesTB
  >> funsTB
