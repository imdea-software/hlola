module StaticAnalysis where

import Lola
import DecDyn
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.List (group, sort)

type Vert=Ident
data FState = St { indexes :: Set.Set Ident, neighbours :: Map.Map Vert [(Vert,[Int])] } deriving Show

processExp :: ExprDyn -> FState -> ([(Vert,[Int])], FState)
processExp (DLeaf _) fs = ([],fs)
processExp (DApp _ e0 e1) fs = let
  (n0, fs0) = processExp e0 fs
  (n1, fs1) = processExp e1 fs0 in
  (n0++n1, fs1)
processExp (DNow decl) fs = ([(dgetId decl,[0])], addDecl fs decl)
processExp (DAt decl (off,e)) fs = let
  (n1, fs1) = processExp e fs
  in
  ((dgetId decl,[off]):n1, addDecl fs1 decl)
processExp (DSlice _ decl e) fs = let
  (n1, fs1) = processExp e fs
  in
  ((dgetId decl,[0..50]):n1, addDecl fs1 decl)

addDecl :: FState -> DeclarationDyn -> FState
addDecl fs (DInp strid) = St (Set.insert strid (indexes fs)) (Map.insert strid [] (neighbours fs))
addDecl fs (DOut (strid,e)) | Set.member strid (indexes fs) = fs -- I've already been processed
                         | otherwise = let
                            newfs = fs {indexes = Set.insert strid $ indexes fs}
                            (neighs, newfs') = processExp e newfs in
                            newfs' { neighbours = Map.insert strid neighs $ neighbours newfs'}

-----------
type FGraph = Map.Map Vert (Map.Map Vert [Int])

getFGraph :: [DeclarationDyn] -> FGraph
getFGraph = getGraph.foldl addDecl initstate
  where
  initstate = St Set.empty Map.empty
  getGraph St{neighbours=themap} = Map.map (Map.fromListWith (\x y -> rmdups (x++y))) themap
  rmdups = map head . group . sort

vertices :: FGraph -> [Vert]
vertices = Map.foldrWithKey (\k _ l -> k:l) []

cycles :: FGraph -> Vert -> [(Int, [Vert])]
cycles g v = paths g Set.empty v v

flattenedges :: FGraph -> [(Vert,Vert,Int)]
flattenedges g = let
  alledges = Map.toList (Map.map Map.toList g) :: [(Vert, [(Vert, [Int])])]
  in concatMap (\(v,ls) -> concatMap (\(v',is) -> map (\i -> (v,v',i)) is) ls) alledges

checkGraph :: FGraph -> Either String ((Int,[Vert]), (Vert,Vert,Int))
-- Left s = Something went wrong
-- Right ((ls, i), (v1,v2,i')) = longest positive path: ls of weight i, most
-- negative edge: v1->v2, of weight i'
checkGraph g = let
  verts = vertices g
  vcycles = map (cycles g) verts
  cyclescheck = mapM_ (foldl checkerr (Right Nothing)) vcycles
  allpairs = [(v,v') | v<-verts, v' <-verts]
  allpaths = concatMap (uncurry $ paths g Set.empty) allpairs
  maxpath = foldl (\(i,ls) (i',ls') -> if i>i' then (i,ls) else (i',ls')) (0,[]) allpaths
  minedge = foldl (\(acv,acv',aci) (v,v',i) -> if i<aci then (v,v',i) else (acv,acv',aci)) ("","",0) (flattenedges g)
  in
  cyclescheck >> return (maxpath,minedge)

checkerr :: Either String (Maybe (Int, [Vert])) -> (Int, [Vert]) -> Either String (Maybe (Int, [Vert]))
checkerr (Left err) _ = Left err
checkerr _ (0, ls) = Left $ "Zero-weighted cycle: " ++ show ls
checkerr (Right Nothing) cyc = Right (Just cyc)
checkerr prev@(Right (Just (wp, lsp))) (w,ls) = if w*wp<0 then Left $ mixedWeights w ls wp lsp else prev

mixedWeights :: (Show a1, Show a2, Show a3, Show a4) => a2 -> a1 -> a4 -> a3 -> String
mixedWeights w ls wp lsp = "Mixed weights: " ++ show ls++ " of weight " ++ show w ++ " and " ++ show lsp ++ " of weight " ++ show wp

paths :: FGraph -> Set.Set Vert -> Vert -> Vert -> [(Int, [Vert])]
paths g forbidden a b = let
    -- We take all the neighbors which have not been visited yet, and are not a itself
    neighs = (g Map.! a) :: Map.Map Vert [Int]
    tl = maybe [] (map (\s -> (s, [a,b]))) $ neighs Map.!? b
    fneighs = Map.filterWithKey (\k _ -> Set.notMember k forbidden && k/=a) neighs
    neighpaths = concatMap (processneighbor (Set.insert a forbidden)) (Map.toList fneighs)
    in neighpaths ++ tl
  where
    processneighbor newfb (n, ws) = let
      ps = paths g newfb n b
      ps' = if n==b then (0,[b]):ps else ps
      in [(w+fst p, a:snd p) | w <- ws, p <- ps']

dotFromGraph :: FGraph -> String
dotFromGraph g = let
  prefix = "digraph {\n"
  suffix =  "}\n"
  entries = flattenedges g
  strings = concatMap (\(v0,v1,i) -> "    " ++ show v0 ++ " -> " ++ show v1 ++ "[label=\"" ++ show i ++ "\"];\n") entries
  in prefix ++ strings ++ suffix
