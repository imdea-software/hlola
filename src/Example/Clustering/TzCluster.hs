{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.Clustering.TzCluster where
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
type Block = (TransactionId, Addr, Addr)
getSrc = snd3
getDest = thd3

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x

rootAddr = "tz1Z7eWGw18LqUgRqmDqNZFQx7f8GEHXRfT8"

-------
spec :: Specification
spec = [out$cluster (rootAddr, Set.empty)]

block :: Stream Block
block = Input "block"

source :: Stream Addr
source = "source" =: getSrc <$> Now block

dest :: Stream Addr
dest = "dest" =: getDest <$> Now block

cluster :: (Addr, Set.Set Addr) -> Stream (Set.Set Addr)
cluster args@(src,init) = "cluster" =: let
  prevcluster = cluster args :@ (-1, Leaf (Set.insert src init))
  newaddresses = fromMaybe Set.empty.listToMaybe.Map.elems <$> cluster `over` newaddrset prevcluster `withInit` myIniter
  in Set.union <$> prevcluster <*> newaddresses

newaddrset :: Expr (Set.Set Addr) -> Stream (Set.Set (Addr, Set.Set Addr))
newaddrset pcluster = "newaddrset" =: let
  maddr = getMissing <$> Now source <*> Now dest <*> pcluster
  in wrap <$> pcluster <*> maddr
  where
  wrap pcluster = maybe Set.empty (\addr -> Set.singleton (addr,pcluster))
  getMissing src dst cluster = let
    res = 1 + fromEnum (src `Set.member` cluster) - fromEnum (dst `Set.member` cluster)
    in [Just src, Nothing, Just dst]!!res

myIniter = undefined
-- myIniter :: Initer (Addr, Set.Set Addr) (Set.Set Addr)
-- myIniter str _ futevs args@(addr,ignoreset) = let
--   prevevs = getprevevs addr
--   in initWithPast (str args) prevevs futevs

getprevevs :: Addr -> [Map.Map Ident Value]
getprevevs addr = unsafePerformIO$getAllJSONs$rawcontentgetter addr

rawcontentgetter :: Addr -> IO B.ByteString
rawcontentgetter addr = readproc "./miprogramilla.py" [addr]

mainpast :: [Map.Map Ident Dynamic]
mainpast = fst$myIniter cluster 0 [] (rootAddr, Set.empty)
