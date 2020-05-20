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
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T
import Data.Dynamic

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
getSystem decs ins = let
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
  past = emptyPast arrsize
  -- to build the system
  focus = Focus past fut
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
  focus = Focus past fut
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

-- Execution

-- Step

-- The function solveFocus converts every Dynamic Expression in the focused
-- instant of the system to a DLeaf that contains a ground value.
-- In the process, it may also solve and convert expressions in future instants.
-- The instants in the past are already Leaves.
solveFocus :: Sys -> Sys
-- If no instant is on focus, then nothing is done
solveFocus sys@(Focus past []) = sys
-- The function solves every Identifier in the focused instant using the
-- auxiliary function solveTop
solveFocus sys@(Focus past (m:_)) = Map.foldlWithKey (\s k _ -> solveTop s k) sys m

-- The function solveTop converts the Dynamic Expression associated to an
-- identifier in the focused instant to a DLeaf. It may solve other expressions
-- in doing so.
solveTop :: Sys -> Ident -> Sys
solveTop sys@(Focus _ (m:_)) id = let
  -- We get the associated expression,
  exp = m Map.! id
  -- we solve it using the auxiliary function solve,
  (newexp, Focus p (h:r)) = solve sys exp in
  -- and we replace the entry in the focused instant
  Focus p ((Map.adjust (const newexp) id h):r)

peek :: Int ->  Sys -> ExprDyn -> Maybe Dynamic
peek _ sys (DLeaf d) = Just d
peek offset sys (DApp tools@(dtolfun, juster, nothing, unlifter) e1 e2) = do
  f <- peek offset sys e1
  peekApply offset sys tools f e2
peek offset sys@(Focus _ (m:_)) (DNow dec) = let
  id = dgetId dec
  exp = m Map.! id
  in
  peek offset sys exp
peek offset sys (DAt dec (i, de))
  | offset + i > 0 = Nothing
  | otherwise = case shiftN i sys of
    Nothing -> peek offset sys de
    Just sys'@(Focus _ (m:_)) -> let
      id = dgetId dec
      exp = m Map.! id
      in
      peek (offset+i) sys' exp

peekApply :: Int -> Sys -> (Dynamic, Dynamic, Dynamic, Dynamic -> Maybe Dynamic) -> Dynamic -> ExprDyn -> Maybe Dynamic
peekApply offset sys (dtolfun, juster, nothing, unlifter) f e2 = let
  -- we peek its argument,
  maybeDynarg = peek offset sys e2 -- Maybe Dynamic <a>
  argDynMaybe = maybe nothing (dynApp juster) maybeDynarg
  -- (DLeaf y, sys'') = solve sys' e2
  mayber = dynApp dtolfun f -- mayber :: Dynamic<Maybe a -> Maybe b>
  dynMaybe = dynApp mayber argDynMaybe -- dynMaybe :: Dynamic <Maybe b>
  maybeDyn = unlifter dynMaybe
  in maybeDyn

-- This function returns the ground value of an expression regarding the focused
-- instant. It may solve and replace other points in doing so.
solve :: Sys -> ExprDyn -> (ExprDyn, Sys)
-- A DLeaf is already a ground value
solve sys x@(DLeaf _) = (x,sys)
-- To solve an application,
solve sys (DApp tools@(dtolfun, juster, nothing, unlifter) e1 e2) = let
  -- we solve the function to apply,
  (DLeaf f, sys') = solve sys e1
  maybeDyn = peekApply 0 sys' tools f e2
  in
  case maybeDyn of
    Just res -> (DLeaf res, sys')
    Nothing -> let
        (DLeaf e2', sys'') = solve sys' e2
        argDynMaybe = dynApp juster e2'
        mayber = dynApp dtolfun f -- mayber :: Dynamic<Maybe a -> Maybe b>
        dynMaybe = dynApp mayber argDynMaybe -- dynMaybe :: Dynamic <Maybe b>
        Just r = unlifter dynMaybe
        in (DLeaf r, sys'')
-- To solve a Now expression,
solve sys (DNow dec) = let
  -- we get its id,
  id = dgetId dec
  -- we use solveTop to solve and replace it,
  Focus p (m:f) = solveTop sys id in
  -- and we look up the result in the map
  (m Map.! id, Focus p (m:f))
-- Finally, to solve the access to a stream in an offset,
solve sys (DAt dec (i, de)) = let
  (solvedexpr, sys') = solve sys de
  in
-- we shift the system as far as the offset mandates,
  case shiftN i sys of
    -- if we fall off the trace, then we use the default value
    Nothing -> (solvedexpr, sys')
    -- otherwise, we behave as we would for the Now expression
    Just shiftedSys -> let
      id = dgetId dec
      Focus p (m:f) = solveTop shiftedSys id in
      -- except that we have to shift back to the present instant
      (m Map.! id, fromMaybe (error "Failed access to past") $ shiftN (-i) (Focus p (m:f)))

-- Output
showCSVRow :: [String] -> String
showCSVRow [] = ""
showCSVRow [x] = x
showCSVRow (x:r) = x ++ "," ++ showCSVRow r

printFocus :: Format -> Sys -> Specification -> String
printFocus f (Focus _ (m:_)) decs = let
  funs = map snd4 decs
  ids = map (dgetId.fst4) decs
  undleaf (DLeaf x) = x
  thedyns = map (undleaf.((Map.!) m) ) ids
  apps = zipWith (\(cf,jf) dyn -> (cf dyn, jf dyn)) funs thedyns
  thejsonmap = Object $ HM.fromList $ zipWith (\id (_,val) -> (T.pack id,val)) ids apps
  line = if f==CSV then showCSVRow $ map fst apps else BS.unpack$encode thejsonmap
  in
  line ++ "\n"

-- We solve the instant in focus and get a String containing the values of the
-- output declarations
procAndPrint :: Format -> Specification -> Sys -> [String]
procAndPrint f decs sys@(Focus _ []) = []
-- If this is the last instant, that is all we do
procAndPrint f decs sys@(Focus _ [_]) = [printFocus f (solveFocus sys) decs]
-- Otherwise, we prepend this string with the result of solving and printing the
-- remaining of the instants in the future, by shifting the system and
-- recomputing
procAndPrint f decs sys = let
  newsys = solveFocus sys
  in (printFocus f newsys decs) : (procAndPrint f decs $ rshift' newsys)

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
run f debug decs ins = let
  (deb, sys) = getSystem (map fst4 decs) ins
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
justProc decs sys@(Focus _ []) = []
justProc decs sys@(Focus _ [_]) = let (Focus _ [(m)]) = solveFocus sys in [Map.map undleaf m]
justProc decs sys@(Focus _ (_:_)) = let (newfocus@(Focus _ ((m):_))) = solveFocus sys in (Map.map undleaf m):(justProc decs (rshift' newfocus))

undleaf (DLeaf x) = x

streamval :: Typeable w => Map.Map String Dynamic -> String -> w
streamval m ix = fromMaybe (error "wrong fromdyn").fromDynamic.fromMaybe (error $ "ix not found: "++ ix) $ Map.lookup ix m

valstream :: Typeable w => String -> Map.Map String Dynamic -> w
valstream = flip streamval
