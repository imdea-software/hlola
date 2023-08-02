{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.Clustering.TzClusterReason where
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
type Arg = (Addr, TransactionId, Map.Map Addr TransactionId)
type Block = (TransactionId, Addr, Addr)
getTransId = fst3
getSrc = snd3
getDest = thd3

type ClusterT = Map.Map Addr TransactionId

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x

rootAddr = "tz1Z7eWGw18LqUgRqmDqNZFQx7f8GEHXRfT8"
-- rootAddr = "tz1bjnczC6tWyeBXie8Ho7jWC2nrNbcUjZ5Y"
rootArg = (rootAddr, -1, Map.empty)

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

cluster :: Arg -> Stream ClusterT
cluster arg@(src,trid,init) = "cluster" =: let
  prevcluster = cluster arg :@ (-1, Leaf (Map.insert src trid init))
  newaddresses = fromMaybe Map.empty.listToMaybe.Map.elems <$> cluster `over` newaddrset arg `withInit` myIniter
  in Map.union <$> prevcluster <*> newaddresses

newaddrset :: Arg -> Stream (Set.Set Arg)
newaddrset arg@(src,trid,init) = "newaddrset" =: let
  pcluster = cluster arg :@ (-1, Leaf (Map.insert src trid init))
  maddr = getMissing <$> Now source <*> Now dest <*> pcluster
  in wrap <$> pcluster <*> Now transid <*> maddr
  where
  wrap pcluster trid = maybe Set.empty (\addr -> Set.singleton (addr,trid,pcluster))
  getMissing src dst cluster = let
    res = 1 + fromEnum (src `Map.member` cluster) - fromEnum (dst `Map.member` cluster)
    in [Just src, Nothing, Just dst]!!res

myIniter = undefined
-- myIniter :: Initer Arg ClusterT
-- myIniter str _ futevs arg@(addr,_,_) = let
--   prevevs = getprevevs addr
--   in initWithPast (str arg) prevevs futevs

getprevevs :: Addr -> [Map.Map Ident Value]
getprevevs addr = unsafePerformIO$getAllJSONs$rawcontentgetter addr

rawcontentgetter :: Addr -> IO B.ByteString
rawcontentgetter addr = readproc "./miprogramilla.py" [addr]

mainpast :: [Map.Map Ident Dynamic]
mainpast = fst$myIniter cluster 0 [] rootArg
