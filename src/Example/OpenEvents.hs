{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.OpenEvents where
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
import Data.Text
import Example.JusterInputs

spec :: Specification
spec = [out not_too_many_events]

open_events :: Stream (Set.Set EventId)
open_events = "open_events" =: let
  fmadd = if Now operation === Leaf NewEvent then Set.insert <$> Now eventId else Leaf id
  fmrm = if Now operation === Leaf Close then Set.delete <$> Now eventId else Leaf id
  in fmadd <*> (fmrm <*> open_events :@ (-1, Leaf Set.empty))


not_too_many_events :: Stream Bool
not_too_many_events = "not_too_many_events" =: (Set.size <$> Now open_events) < 10

