{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.ListedAreOk where
import Data.Aeson
import DecDyn
import GHC.Generics
import Lola
import Syntax.Booleans
import Syntax.HLPrelude
import Syntax.Num
import Syntax.Ord
import Lib.DynPar
import qualified Data.Set as Set
import qualified Prelude as P
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Dynamic
import qualified Data.ByteString.Lazy as B (ByteString)
import TraceRetrieving
import System.IO.Unsafe (unsafePerformIO)
import DecDyn
import Engine.Engine
import Debug.Trace

type TokenId = String

-------
spec :: Specification
spec = [
  -- out thismustcheck,
  out tokenid,
  out level,
  -- out thiswasgenerated,
  -- out thiswassigned,
  out thismanymetadata]

tokenid :: Stream TokenId
tokenid = Input "tokenid"

level :: Stream Int
level = Input "level"

checked :: Stream (Set.Set TokenId)
checked = "checked" =: Set.insert <$> Now tokenid <*> checked :@ (-1, Leaf Set.empty)

thismustcheck :: Stream Bool
thismustcheck = "thismustcheck" =: Set.notMember <$> Now tokenid <*> checked :@ (-1, Leaf Set.empty)

thisok :: Stream Bool
thisok = "thisok" =: (not (Now thismustcheck)) || Now thiswasgenerated

thiswasgenerated :: Stream Bool
thiswasgenerated = "thiswasgenerated" =: wasgenerated <$> Now tokenid

thiswassigned :: Stream Bool
thiswassigned = "thiswassigned" =: wassigned <$> Now tokenid

thismanymetadata :: Stream Int
thismanymetadata = "thishasmetadata" =: howmanymetadata <$> Now tokenid

wasgenerated :: TokenId -> Bool
wasgenerated tok = let
  is = IS undefined falsestrm falsestrm 2
  url = "https://api.tzkt.io/v1/operations/transactions/?target=KT1XYgKrzBbzsckGvXTPgxFyN7KNZ9RPYVWf&entrypoint=generate&select=parameter,id,timestamp,sender,target&parameter=" ++ tok
  args = ["--maxevs", "10", "--maxreqs", "2", url]
  trace = unsafePerformIO (getAllJSONs(readproc fetcher args))
  in isJust (mRunSpec (is `withTrace` trace))

wassigned :: TokenId -> Bool
wassigned tok = let
  is = IS undefined falsestrm falsestrm 2
  url = "https://api.tzkt.io/v1/operations/transactions/?sender=tz1e8XGv6ngNoLt1ZNkEi6sG1A39yF48iwdS&entrypoint=assign_metadata&select=parameter,id,timestamp,sender,target&parameter.token_id=" ++ tok
  args = ["--maxevs", "10", "--maxreqs", "2", url]
  trace = unsafePerformIO (getAllJSONs(readproc fetcher args))
  in isJust (mRunSpec (is `withTrace` trace))

howmanymetadata :: TokenId -> Int
howmanymetadata tok = let
  counter = "counter" =: counter :@ (-1,0) + 1
  counterhigh = "ch" =: Now counter > 4
  is = IS undefined counter counterhigh 3
  url bigmapid = "https://api.tzkt.io/v1/bigmaps/" ++ bigmapid ++ "/keys?active=true&key=" ++ tok
  args bigmapid = ["--maxevs", "10", "--maxreqs", "2", url bigmapid]
  traceof bigmapid = unsafePerformIO (getAllJSONs(readproc fetcher (args bigmapid)))
  trace = traceof "22789"
  -- trace = traceof "411402" ++ traceof "149772" ++ traceof "22789"
  in fromMaybe 0 (mRunSpec (is `withTrace` trace))

falsestrm :: Stream Bool
falsestrm = "false" =: Leaf False
fetcher :: String
fetcher = "/Users/felipe.gorostiaga/Documents/HLola/fxgetter.py"
