{-# LANGUAGE RebindableSyntax  #-}
module Example.DynParEx where
import qualified Prelude as P
import Syntax.HLPrelude
import Syntax.Booleans
import Syntax.Ord
import Syntax.Num
import Lib.Utils
import Lib.DynPar
import Lola
import DecDyn
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

spec :: Specification
spec = [out results]

inpar :: Stream String
inpar = Input "inpar"

params :: Stream (Set.Set String)
params = "params" =: Set.insert <$> Now inpar <*> params :@ (-1, Leaf (Set.empty))

dynstream :: String -> Stream Int
dynstream r = "dynstream" <: r =: Now instantN

results :: Stream (Map.Map String Int)
results = "results" =: dynstream `over` params
