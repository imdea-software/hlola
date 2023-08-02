{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.Blockchain where
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

type Block = (String, String, [String])
getSrc (x,_,_) = x
getDest (_,x,_) = x
getValidating (_,_,x) = x

addr = "0xOURGUY"

-------
spec :: Specification
spec = [out interactors, out all_ok]

block :: Stream Block
block = Input "block"

validatingwallets :: Stream [String]
validatingwallets = "validatingwallets" =: getValidating <$> Now block

interactors :: Stream (Set.Set String)
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

all_ok :: Stream Bool
all_ok = "all_ok" =:
  and <$> (Map.elems <$> kyc_validated `over` interactors)

kyc_validated :: String -> Stream Bool
kyc_validated wallet = "kyc_validated" <: wallet =:
  kyc_validated wallet :@ (-1,Leaf False) || ((elem wallet) <$> Now validatingwallets)
