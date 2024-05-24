{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.ConsistentMetadata where
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
  out tokenId,
  out name,
  out uri,
  out thismetadatauri,
  out lastLevel,
  out consistentMetadata,
  -- out thiswasgenerated,
  -- out thiswassigned,
  out lastTime]

tokenId :: Stream TokenId
tokenId = Input "tokenId"

lastLevel :: Stream Int
lastLevel = Input "lastLevel"

lastTime :: Stream String
lastTime = Input "lastTime"

uri :: Stream String
uri = Input "uri"

name :: Stream String
name = Input "name"

thismetadatauri :: Stream String
thismetadatauri = "thismetadatauri" =: metadatauri <$> Now tokenId

consistentMetadata = "consistentMetadata" =:
  Now name /== Leaf "[WAITING TO BE SIGNED]"
  || (notsignedstr <$> Now thismetadatauri)
  where
  notsignedstr str = any (==str) notsignedstrs
  notsignedstrs = [
    "697066733a2f2f516d525a79796a54654b534159686f7a5a4c76486752506278636d4a51323367704b4b33514e77586b437851436d"
    , "697066733a2f2f516d5a5a56424b617044673277587a77704478646d4c39416836363568395a7a654a3967596462545a3447427a66"
    , "697066733a2f2f516d61615146437538646d61675265313770726e7932706e537577364d707771434a6e4c4e77664e655668515350"
    ]

metadatauri :: TokenId -> String
metadatauri tok = let
  bytes = Input "bytes"
  is = IS undefined bytes ("t" =: Leaf True) 2
  url bigmapid = "https://api.tzkt.io/v1/bigmaps/" ++ bigmapid ++ "/keys?active=true&key=" ++ tok
  args bigmapid = ["--maxevs", "10", "--maxreqs", "2", url bigmapid, "--extract", "value.token_info.", "bytes"]
  traceof bigmapid = unsafePerformIO (getAllJSONs(readproc fetcher (args bigmapid)))
  trace = traceof "411402" ++ traceof "149772" -- ++ traceof "22789"
  in fromMaybe "NO BYTES" (mRunSpec (is `withTrace` trace))

fetcher :: String
fetcher = "/Users/felipe.gorostiaga/Documents/HLola/fxgetter.py"
