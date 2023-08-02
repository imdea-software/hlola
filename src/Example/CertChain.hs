{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.CertChain where
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

type Addr = String
type Block = (Addr, Addr, Bool)
getSrc (x,_,_) = x
getDest (_,x,_) = x
getValidating (_,_,x) = x

addr = "0xOURGUY"
trustedroots = ["0xROOT1", "0xROOT2", "0xROOT3"]

-------
spec :: Specification
spec = [out interactors, out all_ok]

block :: Stream Block
block = Input "block"

validating :: Stream Bool
validating = "validating" =: getValidating <$> Now block

interactors :: Stream (Set.Set Addr)
interactors = "interactors" =: let
  src = getSrc <$> Now block
  dest = getDest <$> Now block
  prevInteractors = interactors :@ (-1, Leaf Set.empty)
  in
  if src === Leaf addr
  then Set.insert <$> dest <*> prevInteractors
  else if dest === Leaf addr
  then Set.insert <$> src <*> prevInteractors
  else prevInteractors

validators :: Addr -> Stream (Set.Set Addr)
validators addr = "validators" <: addr =: let
  prevthis = validators addr :@ (-1, Leaf Set.empty)
  dest = getDest <$> Now block
  src = getSrc <$> Now block
  in
  if Now validating && dest === Leaf addr
  then Set.insert <$> src <*> prevthis
  else prevthis

all_ok :: Stream Bool
all_ok = "all_ok" =:
  and <$> (Map.elems <$> certified `over` interactors)

certified :: Addr -> Stream Bool
certified addr = "certified" <: addr =:
  (elem addr <$> Leaf trustedroots)
  || or <$> (Map.elems <$> certified `over` (validators addr))
