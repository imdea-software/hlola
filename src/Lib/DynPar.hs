{-# LANGUAGE RebindableSyntax  #-}
{-#LANGUAGE TypeOperators#-}
{-# LANGUAGE BangPatterns #-}
{-#LANGUAGE ScopedTypeVariables#-}
module Lib.DynPar where
import Syntax.HLPrelude
import Data.Aeson
import Engine.Engine
import TraceRetrieving
import Engine.Focus
import Lola
import DecDyn
import Syntax.Ord()
import Syntax.Num()
import Syntax.Booleans()
import qualified Prelude as P
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Dynamic
import Data.Default
import Data.Maybe
import qualified Data.Map.Merge.Strict as MM
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)
import Lib.Utils (instantN)
import Type.Reflection
import Data.Typeable (eqT)

data FrozenSys = FS {
  pastfs :: Past Instant,
  outexprs :: Map.Map Ident ExprDyn,
  converter :: [Map.Map Ident Value] -> [Map.Map Ident Dynamic]
  }

refreeze :: FrozenSys -> Sys -> FrozenSys
refreeze oldfs focus = oldfs { pastfs = let !res = past focus in res }

freeze :: [DecDyn] -> Sys -> FrozenSys
freeze decs focus = let 
  outexprs = getOutExprs (map fst4 decs)
  converter = map (checkAndConvert (getFromJSONers decs))
  !res = past focus
  in FS res outexprs converter

unfreeze :: FrozenSys -> [Map.Map Ident Value] -> Sys
unfreeze (FS p outexprs converter) futvals = let
  ins = converter futvals
  inexprs = map (Map.map DLeaf) ins
  fut = map (Map.union outexprs) inexprs
  in
  Focus p fut 0

type PrevRetriever = (Map.Map String Value -> Bool) -> Int -> [Map.Map String Value]

updateall :: Streamable x => Maybe (Expr (Set.Set x))
updateall = Nothing

-- infixl `mover`
-- mover :: (Streamable a, Streamable x, Ord x, Show x, Show a, ToJSON a, ToJSON x, Default x) => (x->Stream a) -> Stream (Maybe x) -> Expr (Maybe a)
-- mover pstr marg = MOver pstr marg (const emptyIniter) updateall mapdyn

infixl `over`
over :: (Streamable a, Streamable x, Ord x, Show x, Show a, ToJSON a, ToJSON x, Default x) => (x->Stream a) -> Stream (Set.Set x) -> Expr (Map.Map x a)
over pstr args = Over pstr args emptyIniter updateall mapdyn

infixl `mover`
mover :: (Streamable a, Streamable x, Ord x, Show x, Show a, ToJSON a, ToJSON x, Default x) => (x->Stream a) -> Stream (Maybe x) -> Expr (Maybe a)
mover pstr marg = MOver pstr marg emptyIniter updateall mmapdyn

infixl `updating`
updating :: Expr (Map.Map x a) -> Expr (Set.Set x) -> Expr (Map.Map x a)
updating (Over pstr args initer _ f) upds = Over pstr args initer (Just upds) f
-- TODO: filter should imply that it belongs to updating (for positive offset references)

infixl `withInit`
withInit :: Expr (Map.Map x a) -> Initer x a -> Expr (Map.Map x a)
withInit (Over pstr args _ mupds f) initer = Over pstr args initer mupds f

infixl `mwithInit`
mwithInit :: Streamable x0 => Expr (Maybe a) -> Initer x0 a -> Expr (Maybe a)
mwithInit (MOver (pstr :: x -> Stream a) marg _ mupds f) (initer :: Initer x0 a) = fromJust themover
  where
  themover = do
    Refl :: x :~: x0 <- eqT
    return $ MOver pstr marg initer mupds f

mmapdyn :: (Streamable a, Show a, ToJSON a) => Expr (Maybe a) -> Expr (Maybe a)
mmapdyn (MOver (pstr :: x -> Stream a) marg initer mupds _) = let
  args = "movertoset" <: marg =: maybe Set.empty Set.singleton <$> Now marg
  theover = Over pstr args initer mupds mapdyn
  themapexpr = mapdyn theover
  in listToMaybe.Map.elems <$> themapexpr

mapdyn :: (Streamable a, Streamable x, Ord x, Show x, Show a, ToJSON a, ToJSON x, Default x) => Expr (Map.Map x a) -> Expr (Map.Map x a)
mapdyn (Over pstr args initer mupds _) = catMaybesMap.Map.map snd <$> Now (monitorsstr initer pstr args (funfor mupds))
  where
  catMaybesMap :: Map.Map k (Maybe v) -> Map.Map k v
  catMaybesMap = Map.map fromJust.Map.filter isJust
  -- funfor :: Maybe (Expr (Set.Set x)) -> Expr ((x -> (FrozenSys, Maybe a) -> (FrozenSys, Maybe a)) -> Map.Map x (FrozenSys, Maybe a) -> Map.Map x (FrozenSys, Maybe a))
  funfor Nothing = Leaf Map.mapWithKey
  funfor (Just eset) = f <$> eset
    where
    f updks updfun inimap = Set.foldr' (Map.adjustWithKey updfun) inimap updks

monitorsstr :: (Streamable a, Streamable x, Ord x, Show x, Show a, ToJSON a, ToJSON x, Default x) => Initer x a -> (x->Stream a) -> Stream (Set.Set x) -> Expr ((x -> (FrozenSys, Maybe a) -> (FrozenSys, Maybe a)) -> Map.Map x (FrozenSys, Maybe a) -> Map.Map x (FrozenSys, Maybe a)) -> Stream (Map.Map x (FrozenSys, Maybe a))
monitorsstr initer str args updater =
  "over_helper" <: str def <: args =: res -- initer agnostic
  where
  prevthis = monitorsstr initer str args updater :@ (-1, Leaf Map.empty)
  res = updater <*> (mupdate str <$> rawin :@@ (-1)) <*> nextmons
  keptmonitors = Map.restrictKeys <$> prevthis <*> Now args
  addedkeys = Set.difference <$> Now args <*> args :@ (-1, Leaf Set.empty)
  addedmonitors = initialize str <$> initer <*> rawin :@@ (-1) <*> addedkeys
  nextmons = Map.union <$> keptmonitors <*> addedmonitors

mupdate :: Streamable a => (x -> Stream a) -> [Map.Map Ident Value] -> x -> (FrozenSys, Maybe a) -> (FrozenSys, Maybe a)
mupdate str raws k v@(frsys,_) = let
  sys = unfreeze frsys raws
  solvedfocus = solveFocus sys
  val = getStream (getId (str k)) (getPresent solvedfocus)
  !newsys = fromJust$rshiftForce 1 solvedfocus
  !frozen = refreeze frsys newsys
  in (frozen, Just $ fromDyn val (error "Wrong dyn in over"))

initialize str inifun evs params = Map.fromSet (\x -> list2frozen (str x) (inifun str evs x)) params

list2frozen :: (ToJSON a, Show a, Streamable a) => Stream a -> ([Map.Map Ident Dynamic], Maybe a) -> (FrozenSys, Maybe a)
list2frozen str (pastlist, mres) = let
  decs = [out str]
  sys = snd$getSystemWithPast pastlist (map fst4 decs) []
  in (freeze decs sys, mres)

sys2list :: Sys -> [Map.Map Ident Dynamic]
sys2list sys = let
  lst = past2list.past$sys
  in map (Map.map undleaf) lst

initWithPast :: (ToJSON a, Show a, Streamable a) => Stream a -> [Map.Map Ident Value] -> [Map.Map Ident Value] -> ([Map.Map Ident Dynamic], Maybe a)
initWithPast strx prevevs futevs = let
  decs = [out strx]
  converter = map (checkAndConvert (getFromJSONers decs))
  instants = converter (prevevs ++ futevs)
  thesys = snd$getSystem (map fst4 decs) instants
  (sys, mdyn) = bringto prevevs thesys strx
  in (sys2list sys, fmap (flip fromDyn (error "Wrong dyn in over")) mdyn)

bringto jsons sys str = bringto' jsons sys Nothing
  where
  bringto' [] sys last = (sys, last)
  bringto' (_:xs) sys _ = let
    solvedfocus = solveFocus sys
    !val = getStream (getId str) (getPresent solvedfocus)
    !newsys = fromJust$rshiftForce 1 solvedfocus
    res = bringto' xs newsys (Just val)
    in res

getPresent (Focus _ (x:_) _) = x
getStream :: Ident -> Instant -> Dynamic
getStream strname instant = undleaf (instant Map.! strname)

rawin :: Stream (Map.Map Ident Value)
rawin = Input "__rawJSON__"

emptyIniter :: (Streamable a, Streamable x) => Initer x a
emptyIniter = Leaf (const$const$const ([], Nothing))


-- Helper for Filters
extract :: (FromJSON a, Streamable a) => Stream a -> Map.Map Ident Value -> a
extract str m = ((fromJust.parseMaybe parseJSON) (m Map.! getId str))

-- Trace initter
-- type Initer x a = Int -> [Map.Map Ident Value] -> x -> ([Map.Map Ident Dynamic], Maybe a)
traceIniter :: (ToJSON a, Show a, Streamable a, Show x) => (x -> [String]) -> ((x -> Stream a) -> [Map.Map Ident Value] -> x -> ([Map.Map Ident Dynamic], Maybe a))
traceIniter argsmaker str evs x = let
  (prog:args) = argsmaker x
  prevevs = unsafePerformIO (getAllJSONs (readproc prog args))
  futevs = evs
  in initWithPast (str x) prevevs futevs
