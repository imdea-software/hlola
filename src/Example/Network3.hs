{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.Network3 where
import Data.Aeson
import DecDyn
import GHC.Generics
import Lola
import Syntax.Booleans
import Syntax.HLPrelude hiding (error)
import Syntax.Num
import Syntax.Ord
import Lib.DynPar
import qualified Data.Set as Set
import qualified Prelude as P
import qualified Data.Map.Strict as Map
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe

data Protocol = TCP | Other deriving (Show,Generic,Read,FromJSON,ToJSON,Eq)
type Address = String

-------
spec :: Specification
spec = [out all_ok, out countdownMap, out attackingPairs]

protocol :: Stream Protocol
protocol = Input "protocol"

syn :: Stream Bool
syn = Input "syn"

ack :: Stream Bool
ack = Input "ack"

source :: Stream Address
source = Input "source"

destination :: Stream Address
destination = Input "destination"

terminatingPairs :: Expr (Set.Set (Address,Address))
terminatingPairs = 
  if not (Now syn) && Now ack
  then Set.singleton <$> ((,) <$> Now source <*> Now destination )
  else Leaf Set.empty

offendingPairs :: Expr (Set.Set (Address,Address))
offendingPairs = 
  if Now syn && not (Now ack)
  then Set.singleton <$> ((,) <$> Now source <*> Now destination)
  else Leaf Set.empty

countdownPairs :: Stream (Set.Set (Address, Address))
countdownPairs = "countdownPairs" =:
  Set.difference <$>
    (Set.difference <$>
      (Set.union <$>
      countdownPairs :@ (-1, Leaf Set.empty)
      <*>
      offendingPairs)
    <*>
    terminatingPairs)
  <*>
  Now attackingPairs

countdownMap :: Stream (Map.Map (Address,Address) Int)
countdownMap = "countdownMap" =: counter `over` countdownPairs

attackingPairs :: Stream (Set.Set (Address, Address))
attackingPairs = "attackingPairs" =:
  Set.difference <$>
    (Set.union <$>
    attackingPairs :@ (-1, Leaf Set.empty)
    <*>
    (Map.keysSet.Map.filter (P.>= 3) <$> countdownMap :@ (-1, Leaf Map.empty)))
  <*>
  terminatingPairs

all_ok :: Stream Bool
all_ok = "all_ok" =:
  (P.<10).Set.size <$> Now attackingPairs
  &&
  all (P.<5).Map.elems <$> counter `over` attackingPairs -- `when` myfilter

myfilter (src,dest) m =
  src == extract source m
  P.&&
  dest == extract destination m

counter :: (Address,Address) -> Stream Int
counter sd = "counter" <: sd =: counter sd :@ (-1,0) + 1

-- Taken from https://www.react.uni-saarland.de/publications/ffst16.pdf (Fig. 2):
-- input string Protocol, Syn, Ack, Source, Destination
-- output (string,string) incompleteHandshakeInvoke:
-- ext: Protocol= "TCP" & Syn="Set" & Ack="Not Set";
-- := (Source,Destination)
--
-- output bool incompleteHandshakeTerminate<src, dst>:
-- inv: incompleteHandshakeInvoke;
-- =Source=src & Destination=dst & Syn="Not Set" & Ack="Set"
--
-- output int waitForAck<src,dst>:
-- inv: incompleteHandshakeInvoke;
-- ter: incompleteHandshakeTerminate
-- = waitForAck(src, dst)[-1,0]+1
--
-- output (string,string) tcpSynInvoke<src, dst>:
-- inv: incompleteHandshakeInvoke;
-- ext: waitForAck(src,dst)[0,0] > threshold
-- ter: waitForAck(src,dst)[0,0] > threshold
-- = (src,dst)
--
-- output bool tcpSynExtend<src,dst>:
-- inv:tcpSynInvoke;
-- = src = Source & dst = Destination & Syn = "Set"
--
-- output bool tcpSynTerminate<src,dst>:
-- inv:tcpSynInvoke;
-- = src = Source & dst = Destination & Syn = "Not Set" & Ack="Set"
--
-- output int tcpSynScan<src,dst>:
-- inv:tcpSynInvoke;
-- ext:tcpSynExtend;
-- ter:tcpSynTerminate;
-- =tcpSynScan(src,dst)[-1,0] +1
--
-- trigger count(tcpSynScan) > threshold2 trigger any(tcpSynScan > threshold3)
