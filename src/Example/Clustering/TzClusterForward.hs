{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.Clustering.TzClusterForward where
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

type TransactionId = Int
type Addr = String
type Arg = (Addr, TransactionId, Set.Set Addr)
type Block = (TransactionId, Addr, Addr)
getTransId = fst3
getSrc = snd3
getDest = thd3

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x

rootAddr = "tz1Z7eWGw18LqUgRqmDqNZFQx7f8GEHXRfT8"
-- rootAddr = "tz1bjnczC6tWyeBXie8Ho7jWC2nrNbcUjZ5Y"
rootArg = (rootAddr, -1, Set.empty)

-------
spec :: Specification
spec = [out$cluster rootArg]

block :: Stream Block
block = Input "block"

source :: Stream Addr
source = "source" =: getSrc <$> Now block

dest :: Stream Addr
dest = "dest" =: getDest <$> Now block

transid :: Stream TransactionId
transid = "transid" =: getTransId <$> Now block

cluster :: Arg -> Stream (Set.Set Addr)
cluster arg@(src,_,init) = "cluster" =: let
  prevcluster = cluster arg :@ (-1, Leaf (Set.insert src init))
  newaddresses = fromMaybe Set.empty.listToMaybe.Map.elems <$> cluster `over` newaddrset arg `withInit` myIniter
  in Set.union <$> prevcluster <*> newaddresses

newaddrset :: Arg -> Stream (Set.Set Arg)
newaddrset arg@(src,_,init) = "newaddrset" =: let
  pcluster = cluster arg :@ (-1, Leaf (Set.insert src init))
  missing = getMissing <$> Now source <*> Now dest <*> pcluster
  maddr = if Now (activated arg) then missing else Leaf Nothing
  in wrap <$> pcluster <*> Now transid <*> maddr
  where
  wrap pcluster trid = maybe Set.empty (\addr -> Set.singleton (addr,trid,pcluster))
  getMissing src dst cluster = let
    res = 1 + fromEnum (src `Set.member` cluster) - fromEnum (dst `Set.member` cluster)
    in [Just src, Nothing, Just dst]!!res

activated :: Arg -> Stream Bool
activated arg@(_,trid,_) = "activated" =: activated arg :@ (-1, Leaf False) || Leaf trid >= Now transid || Leaf trid === -1

myIniter = undefined
-- myIniter :: Initer Arg (Set.Set Addr)
-- myIniter str _ futevs arg@(addr,_,_) = let
--   prevevs = getprevevs addr
--   in initWithPast (str arg) prevevs futevs

getprevevs :: Addr -> [Map.Map Ident Value]
getprevevs addr = unsafePerformIO$getAllJSONs$rawcontentgetter addr

rawcontentgetter :: Addr -> IO B.ByteString
rawcontentgetter addr = readproc "./miprogramilla.py" [addr]

mainpast :: [Map.Map Ident Dynamic]
mainpast = fst$myIniter cluster 0 [] rootArg
