{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.AcceptableUptimes where
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

spec :: Specification
spec = [out acceptable_uptimes]

open_events :: Stream (Set.Set EventId)
open_events = "open_events" =: let
  fmadd = if Now operation === Leaf NewEvent then Set.insert <$> Now eventId else Leaf id
  fmrm = if Now operation === Leaf Close then Set.delete <$> Now eventId else Leaf id
  in fmadd <*> (fmrm <*> open_events :@ (-1, Leaf Set.empty))

uptime :: EventId -> Stream UTCTime
uptime ev = "uptime" <: ev =: uptime ev :@ (-1, Now time)

acceptable_uptimes :: Stream Bool
acceptable_uptimes = "acceptable_uptimes" =: 
  checkdiffs <$> Now time <*> uptime `over` open_events

checkdiffs :: UTCTime -> Map.Map EventId UTCTime -> Bool
checkdiffs ts = all ((P.<240*60*60).floor.diffUTCTime ts).Map.elems

diffs :: Stream (Map.Map EventId Integer)
diffs = "diffs" =: 
  makediff <$> Now time <*> uptime `over` open_events
  where
  makediff ts m = Map.map (\tt -> floor$diffUTCTime ts tt) m

maxdif :: Stream Integer
maxdif = "maxdif" =: maximum.Map.elems <$> Now diffs

