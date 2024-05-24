module Engine.Engine where

import Lola
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as MM
import Engine.Focus
import Data.Maybe
import StaticAnalysis
import DecDyn
import Data.Tuple.Utils
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Key (fromString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T
import Data.Dynamic
import Debug.Trace

-- An instant represents the value of all the streams at one point.
-- It is a map that associates the identifier of the stream to its expression.
type Instant = Map.Map Ident ExprDyn
-- A system is a stream of instants, where one of them has the focus.
-- Since an Instant is a Map from Identifier to Expression, we can think of a
-- System as a matrix of size (nxm) -- where n is the amount of instants and m is
-- the amount of streams -- where one column represents the current instant.
type Sys = Focus Instant
type DebugInfo = (String, Int, [Vert], Vert, Vert, Int)

-- Given a specification and a list of values, we will create a system with an
-- empty past and the focus on the first instant of the system.
-- If we inspect the system at this time, we will find that, for every input
-- stream i and instant n, the value at (i,n) in the System matrix is a DLeaf
-- containing the value of the input i at instant n; while the value of every
-- output stream o and instant n is the ExprDyn defined in the specification:
--        0            1            2            3
--  i0  Leaf v{0,0}  Leaf v{0,1}  Leaf v{0,2}  Leaf v{0,3}
--  i1  Leaf v{1,0}  Leaf v{1,1}  Leaf v{1,2}  Leaf v{1,3}
--  i2  Leaf v{2,0}  Leaf v{2,1}  Leaf v{2,2}  Leaf v{2,3}      ...
--  o0  Expr o0      Expr o0      Expr o0      Expr o0
--  o1  Expr o1      Expr o1      Expr o1      Expr o1
--  o2  Expr o2      Expr o2      Expr o2      Expr o2
--
-- At this moment, we also calculate the dependency graph of a specification, we
-- check that it is well-defined, we calculate the minimum (most negative) edge
-- and the max (most positive) path on it, and use these numbers to compute the
-- size of the Past array required to run the system.
getSystem :: [DeclarationDyn] -> [Map.Map Ident Dynamic] -> (DebugInfo, Sys)
getSystem = getSystemWithPast []

getSystemWithPast :: [Map.Map Ident Dynamic] -> [DeclarationDyn] -> [Map.Map Ident Dynamic] -> (DebugInfo, Sys)
getSystemWithPast pastlist decs ins = let
  -- g is the dependency graph of the specification
  g = getFGraph decs
  -- dot is the dotfile to plot the dependency graph
  dot = dotFromGraph g
  -- We check the graph and calculate the fan and the latency
  ((maxlen,maxpath),(minv0,minv1,minedge)) =
    case checkGraph g of
        Right x -> x
        Left err -> error err
  -- We combine these values to get the array size
  arrsize = maxlen - minedge
  -- and create the empty past
  epast = emptyPast arrsize
  past = foldl (flip pastCons) epast (map (Map.map DLeaf) pastlist)
  -- to build the system
  focus = Focus past fut 0
  -- We get the expression of each output stream (the bottom part of the first
  -- column in the example)
  outexprs = getOutExprs decs
  -- We get the expressions of each input stream at each instant (the top rows
  -- of the matrix in the example)
  inexprs = map (Map.map DLeaf) ins
  -- We define a function to output an error if a key is both an input and an
  -- output
  errDupKeys = MM.zipWithMatched (\k _ _ -> error $ "Duplicate key: " ++ show k)
  -- We add (merge) the output expressions to every instant
  fut = map (MM.merge MM.preserveMissing MM.preserveMissing errDupKeys outexprs) inexprs
  in
  ((dot, maxlen, maxpath, minv0, minv1, minedge), focus)

getHintedSystem :: Int -> [DeclarationDyn] -> [Map.Map Ident Dynamic] -> Sys
getHintedSystem hint decs ins = let
  arrsize = hint
  past = emptyPast arrsize
  outexprs = getOutExprs decs
  inexprs = map (Map.map DLeaf) ins
  errDupKeys = MM.zipWithMatched (\k _ _ -> error $ "Duplicate key: " ++ show k)
  fut = map (MM.merge MM.preserveMissing MM.preserveMissing errDupKeys outexprs) inexprs
  focus = Focus past fut 0
  in
  focus

getOutExprs :: [DeclarationDyn] -> Map.Map Ident ExprDyn
-- We export every declaration to Dynamic and add it to a map associated to
-- their ids
getOutExprs decs = foldl (\m dec -> addDec m dec) Map.empty decs

addDec :: Map.Map Ident ExprDyn -> DeclarationDyn -> Map.Map Ident ExprDyn
-- The input declarations are not added to the map
addDec m (DInp id) = m
addDec m (DOut (id,exp))
-- If an output declaration has already been added, then it is ignored
    | Map.member id m = m
-- We first insert the declaration in the map and call recursively the addition
-- of declarations in its body
    | otherwise = let newmap = Map.insert id exp m
      in addExp newmap exp

addExp :: Map.Map Ident ExprDyn -> ExprDyn -> Map.Map Ident ExprDyn
addExp m (DApp _ e1 e2) = let m' = addExp m e1 in addExp m' $ e2
addExp m (DLeaf _) = m
addExp m (DNow dec) = addDec m dec
addExp m (DAt dec (_,de)) = let m' = addExp m de in addDec m' dec
addExp m (DSlice _ dec de) = let m' = addExp m de in addDec m' dec

-- Execution

-- Step

-- The function solveFocus converts every Dynamic Expression in the focused
-- instant of the system to a DLeaf that contains a ground value.
-- In the process, it may also solve and convert expressions in future instants.
-- The instants in the past are already Leaves.
solveFocus :: Sys -> Sys
-- If no instant is on focus, then nothing is done
solveFocus sys@(Focus past [] _) = sys
-- The function solves every Identifier in the focused instant using the
-- auxiliary function solveTop
solveFocus sys@(Focus past (m:_) _) = Map.foldlWithKey (\s k _ -> solveTop s k) sys m

-- The function solveTop converts the Dynamic Expression associated to an
-- identifier in the focused instant to a DLeaf. It may solve other expressions
-- in doing so.
solveTop :: Sys -> Ident -> Sys
solveTop sys@(Focus _ (m:_) _) id = let
  -- We get the associated expression,
  exp = Map.findWithDefault (error$"Could not find stream \"" ++ id ++ "\"") id m
  -- we solve it using the auxiliary function solve,
  (newexp, Focus p (h:r) ni) = solve sys exp in
  -- and we replace the entry in the focused instant
  Focus p ((Map.adjust (const newexp) id h):r) ni

mymapget m k = fromMaybe (error ("missing key: "++k)) (Map.lookup k m)

peek :: Sys -> ExprDyn -> (Maybe Dynamic, Sys)
peek sys (DLeaf d) = (Just d, sys)
peek sys (DApp tools@(_, juster, nothing, unlifter) e1 e2) = let
  (mf, sys') = peek sys e1
  in if isNothing mf then (Nothing, sys') else peekApply sys' tools (fromJust mf) e2
peek sys@(Focus _ (m:_) _) (DNow dec) = let
  id = dgetId dec
  exp = m `mymapget` id
  (ret, sys'@(Focus sa (sm:sb) sc)) = peek sys exp
  in
  (ret, maybe sys' (\d -> Focus sa (Map.insert id (DLeaf d) sm:sb) sc) ret)
peek sys (DAt dec (i, de))
  | i > offset = (Nothing, sys)
  | otherwise = case shiftN i sys of
    Nothing -> peek sys de
    Just sys'@(Focus _ (m:_) _) -> let
      id = dgetId dec
      exp = m `mymapget` id
      (ret, sys'') = peek sys' exp
      in
      (ret, fromMaybe (error "wrong shift back") $ shiftN (-i) sys'')
    where offset = intoTheFut sys
peek sys slice@(DSlice tools _ dlen)
  | isNothing mlen = (Nothing, sys)
  | otherwise = let
    (d,sys'') = solve sys' slice
    in (Just (undleaf d), sys'')
    where
      offset = intoTheFut sys
      (mlen, sys') = peek sys dlen
      Just lend = mlen
      len = fromDyn lend (error "error len") :: Int

listofmaybe2maybelist :: (Dynamic, Dynamic -> Dynamic -> Dynamic) -> [Maybe Dynamic] -> Maybe Dynamic
listofmaybe2maybelist (dnil, _) [] = Just dnil
listofmaybe2maybelist _ (Nothing:_) = Nothing
listofmaybe2maybelist tools@(_, dcons) (Just v:rest) = case listofmaybe2maybelist tools rest of
  Nothing -> Nothing
  Just l -> Just (dcons v l)

peekApply :: Sys -> ((Dynamic, Dynamic), Dynamic, Dynamic, Dynamic -> Maybe Dynamic) -> Dynamic -> ExprDyn -> (Maybe Dynamic, Sys)
peekApply sys ((dtolfun, dmayber), juster, nothing, unlifter) f e2 = let
  -- we peek its argument,
  (maybeDynarg, sys') = peek sys e2 -- Maybe Dynamic <a>
  argDynMaybe = maybe nothing (dynApp juster) maybeDynarg
  -- (DLeaf y, sys'') = solve sys' e2
  dlfun = case dynApply dtolfun f of
    Just f' -> f'
    Nothing -> f
  mayber = dynApp dmayber dlfun -- mayber :: Dynamic<Maybe a -> Maybe b>
  dynMaybe = dynApp mayber argDynMaybe -- dynMaybe :: Dynamic <Maybe b>
  maybeDyn = unlifter dynMaybe
  in (maybeDyn, sys')

-- This function returns the ground value of an expression regarding the focused
-- instant. It may solve and replace other points in doing so.
solve :: Sys -> ExprDyn -> (ExprDyn, Sys)
-- A DLeaf is already a ground value
solve sys x@(DLeaf _) = (x,sys)
-- To solve an application,
solve sys expr@(DApp tools@(_, juster, nothing, unlifter) e1 e2) = let
  -- we solve the function to apply,
  (DLeaf f, sys'') = solve sys e1
  (maybeDyn, sys') = peekApply sys'' tools f e2
  in
  case maybeDyn of
    Just res -> (DLeaf res, sys')
    Nothing -> solve (sys' {intoTheFut = intoTheFut sys' + 1}) expr
-- To solve a Now expression,
solve sys (DNow dec) = let
  -- we get its id,
  id = dgetId dec
  -- we use solveTop to solve and replace it,
  nsys = solveTop sys id
  Focus _ (m:f) _ = nsys in
  -- and we look up the result in the map
  (m Map.! id, nsys)
-- Finally, to solve the access to a stream in an offset,
solve sys (DAt dec (i, de)) = let
  (solvedexpr, sys') = solve sys de
  in
-- we shift the system as far as the offset mandates,
  case shiftNZero i sys of
    -- if we fall off the trace, then we use the default value
    Nothing -> (solvedexpr, sys')
    -- otherwise, we behave as we would for the Now expression
    Just shiftedSys -> let
      id = dgetId dec
      nsys = solveTop shiftedSys id
      Focus _ (m:f) _ = nsys in
      -- except that we have to shift back to the present instant
      (m Map.! id, fromMaybe (error "Failed access to pastito") $ shiftN (-i) nsys)
solve sys (DSlice (nilList, dcons,tolistdyn) dec dlen) = let
  (solvedexpr, sys') = solve sys dlen
  len = fromDyn (undleaf solvedexpr) (error "not int in slice") :: Int
  (sliceres, sysres) = getSlice len sys'
  in
  (DLeaf $ tolistdyn $ sliceres, sysres)
  where
  sid = dgetId dec
  getSlice n sys
    | n == 0 = ([], sys)
    | n == 1 = ([undleaf nowval], nsys)
    | otherwise = let
      (innerlist, innersys) = case shiftNZero 1 sys of
        Nothing -> ([], sys) 
        Just shiftedsys -> let (il, is) = getSlice (n-1) shiftedsys in (il, fromMaybe (error "Failed access to past") $ shiftN (-1) is)
      in
      ((undleaf nowval):innerlist, nsys)
    where
    nsys = solveTop sys sid
    Focus _ (m:_) _ = nsys
    nowval = m Map.! sid

-- Output
showCSVRow :: [String] -> String
showCSVRow [] = ""
showCSVRow [x] = x
showCSVRow (x:r) = x ++ "," ++ showCSVRow r

printFocus :: Format -> Sys -> Specification -> String
printFocus f (Focus _ (m:_) _) decs = let
  funs = map snd4 decs
  ids = map (dgetId.fst4) decs
  undleaf (DLeaf x) = x
  thedyns = map (undleaf.(mymapget m) ) ids
  apps = zipWith (\(cf,jf) dyn -> (cf dyn, jf dyn)) funs thedyns
  thejsonmap = Object $ KM.fromList $ zipWith (\id (_,val) -> (fromString id,val)) ids apps
  line = if f==CSV then showCSVRow $ map fst apps else BS.unpack$encode thejsonmap
  in
  line ++ "\n"

-- We solve the instant in focus and get a String containing the values of the
-- output declarations
procAndPrint :: Format -> Specification -> Sys -> [String]
procAndPrint f decs sys@(Focus _ [] _) = []
-- If this is the last instant, that is all we do
-- procAndPrint f decs sys@(Focus _ [_] _) = [printFocus f (solveFocus sys) decs]
-- REMOVED LINE, it made the engine wait to decide which pattern to match even though
-- they start with the same element.
-- Otherwise, we prepend this string with the result of solving and printing the
-- remaining of the instants in the future, by shifting the system and
-- recomputing
procAndPrint f decs sys@(Focus _ (_:ls) _) = let
  newsys = solveFocus sys
  in (printFocus f newsys decs) : if null ls then [] else (procAndPrint f decs $ rshift' newsys)

rshift' sys = case shiftN 1 sys of
  Just s -> s
  Nothing -> sys

showDebug :: DebugInfo -> String
showDebug (dot, maxlen, maxpath, minv0, minv1, minedge) =
  "Dot file:\n" ++ dot ++
  "Max path: " ++ show maxpath ++
  "\nof length " ++ show maxlen ++
  ".\nMin vertex: " ++ show minv0 ++
  " -> " ++ show minv1 ++
  " of length "++ show minedge ++ ".\n"

run :: Format -> Bool -> Specification -> [Map.Map Ident Dynamic] -> String
run = runWithPast []

runWithPast :: [Map.Map Ident Dynamic] -> Format -> Bool -> Specification -> [Map.Map Ident Dynamic] -> String
runWithPast pastlist f debug decs ins = let
  (deb, sys) = getSystemWithPast pastlist (map fst4 decs) ins
  header = if f==CSV then showCSVRow (map (dgetId.fst4) decs) ++ "\n" else ""
  outsys = concat $ procAndPrint f decs sys in
  if debug then showDebug deb ++ header ++ outsys else outsys

runLib :: Specification -> [Map.Map Ident Dynamic] -> [Map.Map Ident Dynamic]
runLib decs ins = let
  (_, sys) = getSystem (map fst4 decs) ins in
  justProc decs sys

runHintedLib :: Int -> Specification -> [Map.Map Ident Dynamic] -> [Map.Map Ident Dynamic]
runHintedLib hint decs ins = let
  sys = getHintedSystem hint (map fst4 decs) ins in
  justProc decs sys

justProc :: Specification -> Sys -> [Map.Map Ident Dynamic]
justProc decs sys@(Focus _ [] _) = []
justProc decs sys@(Focus _ [_] _) = let (Focus _ [m] _) = solveFocus sys in [Map.map undleaf m]
justProc decs sys@(Focus _ (_:_) _) = let (newfocus@(Focus _ (m:_) _)) = solveFocus sys in (Map.map undleaf m):(justProc decs (rshift' newfocus))

undleaf (DLeaf x) = x

runSpec :: Typeable a => InnerSpecification a -> a
runSpec innerspec = fromMaybe (error "Innerspec executed with empty input") (mRunSpec innerspec)

mRunSpec :: Typeable a => InnerSpecification a -> Maybe a
mRunSpec innerspec = let
  decs = getDecs innerspec
  ins = getIns innerspec
  sys = getHintedSystem (hint innerspec) (map fst4 decs) ins in
  procWithRet innerspec sys

procWithRet :: Typeable a => InnerSpecification a -> Sys -> Maybe a
procWithRet innerspec sys@(Focus _ [] _) = Nothing
procWithRet innerspec sys@(Focus _ [_] _) = let (_, _, ret) = solveFocusWithRet innerspec sys in Just ret
procWithRet innerspec sys@(Focus _ (_:fut) _) = let
  (newfocus, stop, ret) = solveFocusWithRet innerspec sys
  procrest = procWithRet innerspec (rshift' newfocus)
  in if stop then Just ret else Just (fromMaybe ret procrest)

solveFocusWithRet :: Typeable a => InnerSpecification a -> Sys -> (Sys, Bool, a)
solveFocusWithRet innerspec sys = let
  newsys@(Focus _ (m:_) _) = solveFocus sys
  retStreamId = getId $ retStream innerspec
  stopStreamId = getId $ stopStream innerspec
  DLeaf dret = m Map.! retStreamId
  DLeaf dstop = m Map.! stopStreamId
  ret = getFromDynner innerspec dret
  stop = fromDyn dstop undefined :: Bool
  in (newsys, stop, ret)

streamval :: Typeable w => Map.Map String Dynamic -> String -> w
streamval m ix = fromMaybe (error "wrong fromdyn").fromDynamic.fromMaybe (error $ "ix not found: "++ ix) $ Map.lookup ix m

valstream :: Typeable w => String -> Map.Map String Dynamic -> w
valstream = flip streamval
