{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.Network where
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

data Protocol = HTTP | Other deriving (Show,Generic,Read,FromJSON,ToJSON,Eq)
data ResponsePhrase = OK | BadRequest | NotFound  deriving (Show,Generic,Read,FromJSON,ToJSON,Eq)
type Address = String

-------
spec :: Specification
spec = [out all_ok]

protocol :: Stream Protocol
protocol = Input "protocol"

responsephrase :: Stream ResponsePhrase
responsephrase = Input "responsephrase"

source :: Stream Address
source = Input "source"

destination :: Stream Address
destination = Input "destination"

badhttpers :: Stream (Set.Set (String, String))
badhttpers = "badhttpers" =: let
  oldthis = badhttpers :@ (-1, Leaf Set.empty)
  srcdest = (,) <$> Now source <*> Now destination
  in
  if Now responsephrase === Leaf OK then
  Set.delete <$> srcdest <*> oldthis 
  else
  Set.insert <$> srcdest <*> oldthis 

counter :: (String, String) -> Stream Int
counter srcdest = "counter" <: srcdest =: 
  counter srcdest :@ (-1,0) + 1

all_ok :: Stream Bool
all_ok = "all_ok" =:
  all (P.<3).Map.elems <$> counter `over` badhttpers -- `when` myfilter
  `updating` (updset <$> Now source <*> Now destination)
  where updset src dest = Set.singleton (src, dest)

myfilter (src,dest) m =
  src == extract source m
  P.&&
  dest == extract destination m

-- Taken from https://www.react.uni-saarland.de/publications/ffst16.pdf (Fig. 1):
-- input string Protocol, RequestMethod, ResponsePhrase, Source, Destination
--
-- output (string, string) badHttpRequestInvoke;
-- ext: Protocol="HTTP" & (ResponsePhrase="Bad Request" | "Not Found")
-- := (Source, Destination)
--
-- output bool badHttpRequestExtend<src, dst>:
-- inv: badHttpRequestInvoke;
-- := src=Source & dst=Destination & ResponsePhrase = "Bad Request" | "Not Found"
--
-- output bool webApplicationFingerprintingTerminate<src,dst>:
-- inv: badHttpRequestInvoke;
-- := src=Source & dst=Destination & ResponsePhrase = "OK"
--
-- output int webApplicationFingerprinting<src, dst>:
-- inv: badHttpRequestInvoke;
-- ext: badHttpRequestExtend;
-- ter: webApplicationFingerprintingTerminate
-- := webApplicationFingerprinting(src, dst)[-1,0]+1
--
-- trigger any(webApplicationFingerprinting > threshold)
