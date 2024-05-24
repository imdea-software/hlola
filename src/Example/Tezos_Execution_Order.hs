{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.Tezos_Execution_Order where
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
import Engine.Engine
import Interpreter.TzExecutionTheory

tx_src :: Stream [String]
tx_src = Input "tx_src"

tx_dst :: Stream [String]
tx_dst = Input "tx_dst"

mode :: Stream Mode
mode = Input "mode"

tx :: Stream [Transaction]
tx = "tx" =:
  map (\(s,d) -> TX s d) <$> lists
  where
    sr = Now tx_src
    ds = Now tx_dst
    lists = zip <$> sr <*> ds

spec :: Specification
spec = out are_equal_exec : [out gen_all] -- : out are_equal_exec : [out call_gen_trees]

gen_all :: Stream [Tree]
gen_all = "gen_all" =:
  gen_all_trees <$> Now tx <*> Now mode
  
call_gen_trees :: Stream Int
call_gen_trees = "call_gen_trees" =:
  length <$> (gen_all_trees <$> Now tx <*> Now mode)

are_equal_exec :: Stream Bool
are_equal_exec = "are_equal_exec" =:
  are_equal <$> Now tx <*> Now mode
 


