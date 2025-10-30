{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.StateWithdrawn where
import Data.Aeson
import GHC.Generics
import Data.Time
import DecDyn
import Lola
import Syntax.Booleans
import Syntax.HLPrelude
import Syntax.Num
import Syntax.Ord
import qualified Data.Set as Set
import Data.Text (unpack)
import Data.Time
import qualified Data.Map.Strict as Map
import qualified Prelude as P
import Lib.DynPar
import Example.JusterInputs

data State = Idle | Fault | Open | Closed deriving (Show,Generic,Read,FromJSON,ToJSON,Eq)

spec :: Specification
spec = [out correct_withdraw, out eventId, out transId]

time :: Stream UTCTime
time = "time" =:
  parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" <$> Now timestamp

check_event :: Stream (Set.Set EventId)
check_event = "check_event" =:
  if Now operation === Leaf Withdraw then Set.singleton <$> Now eventId else Leaf Set.empty

correct_withdraw :: Stream Bool
correct_withdraw = "correct_withdraw" =:
  all (== Closed).Map.elems <$> state `over` check_event

state :: EventId -> Stream State
state ev = "state" <: ev =: let
  prevstate = state ev:@(-1, Leaf Idle)
  irrelevant = Now eventId /== Leaf ev || Now operation === Leaf Other
  faulty = prevstate === Leaf Fault
  in
  if faulty || irrelevant then prevstate else
  nextstate <$> prevstate <*> Now operation
  where
  nextstate Idle NewEvent = Open
  nextstate Open Bet = Open
  nextstate Open Close = Closed
  nextstate Closed Withdraw = Closed
  nextstate _ _ = Fault

