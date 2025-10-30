{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.AskedWasMinted where
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

type Addr = String

mktaddr = "KT1FvqJwEDWb1Gwc55Jd1jjTHRVWbYKUUpyq"
mfaddr = "KT1Aq4wWmVanpQhq4TTfjZXB5AjFpx15iQMM"

-------
spec :: Specification
spec = [out all_ok, out askedfor]

askedfor :: Stream Addr
askedfor = Input "askedfor"

target :: Stream Addr
target = Input "target"

entrypoint :: Stream Addr
entrypoint = Input "entrypoint"

sender :: Stream Addr
sender = Input "sender"

all_ok :: Stream Bool
all_ok = "all_ok" =:
  and <$> wasminted `over` askingfor `withInit` myIniter "mint"

alreadyaskedfor :: Stream (Set.Set Addr)
alreadyaskedfor = "alreadyaskedfor" =:
 Set.insert <$> Now askedfor <*> alreadyaskedfor :@ (-1,Leaf Set.empty)

askingfor :: Stream (Set.Set Addr)
askingfor = "askingfor" =:
  if
  Now target === Leaf mktaddr
  && Now entrypoint === Leaf "ask"
  && not (Set.member <$> Now askedfor <*> alreadyaskedfor :@ (-1,Leaf Set.empty))
  then Set.singleton <$> Now askedfor
  else Leaf$Set.empty

wasminted :: Addr -> Stream Bool
wasminted addr = "wasminted" <: addr =:
  wasminted addr :@ (-1,Leaf False) ||
  (Now target === Leaf addr &&
  Now sender === Leaf mfaddr &&
  Now entrypoint === Leaf "mint")

myIniter = undefined
-- myIniter :: String -> Initer Addr Bool
-- myIniter operation str _ futevs addr = let
--   prevevs = getprevevs addr operation
--   in initWithPast (str addr) prevevs futevs

getprevevs :: Addr -> String -> [Map.Map Ident Value]
getprevevs addr operation = unsafePerformIO$getAllJSONs$rawcontentgetter addr operation

rawcontentgetter :: Addr -> String -> IO B.ByteString
rawcontentgetter addr operation = readproc "./minting.py" [addr, operation]
