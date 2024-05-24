{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-#LANGUAGE TypeApplications #-}
module Interpreter.TzExecutionTheory where
import Interpreter.Theory
import Prelude
import GHC.Generics
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Aeson
import Data.Aeson.Types
import Data.Default
import Data.Proxy

type Vertex = Int
type Address = String

type Label = [Address]
type Parent = [Vertex]
type Children = [[Vertex]]

data Mode = BFS | DFS deriving (Show,Generic,Read,FromJSON,ToJSON,Eq,Ord)

data Transaction = TX { source :: Address, dest :: Address } deriving (Show,Generic,Read,FromJSON,ToJSON,Eq,Ord)

data Tree = TR { label :: Label, parent :: Parent, children :: Children, size :: Int } deriving (Show,Generic,Read,FromJSON,ToJSON,Eq,Ord)

instance Default Mode where
  def = BFS

instance Default Transaction where
  def = TX "" ""

instance Default Tree where
  def = TR [] [] [] 0

-- create new tree
new_tree :: Address -> Tree
new_tree new_addr =
  TR [new_addr] [0] [[]] 1

-- add node to tree
add_node :: Tree -> Address -> Vertex -> Tree
add_node tree new_addr new_vertex_dad =
  TR new_label new_parent new_children $ (size tree) + 1
  where
    v = size tree
    new_label = (label tree) ++ [new_addr]
    new_parent = (parent tree) ++ [new_vertex_dad]
    pre_child = take new_vertex_dad (children tree)
    post_child = drop (new_vertex_dad + 1) (children tree)
    actual_child = (children tree) !! new_vertex_dad
    new_children = pre_child ++ [(actual_child ++ [v])] ++ post_child ++ [[]]

search_dads :: Tree -> Mode -> [Vertex]
search_dads t BFS =
  [init..end]
  where
    end = (size t) - 1
    init = (parent t) !! end
search_dads (TR _ _ _ 1) DFS = [0]
search_dads t DFS =
  [((size t) - 1)] ++ (search_dads pruned_tree DFS)
  where
    n = (last (parent t)) + 1
    pr_label = take n (label t)
    pr_parent = take n (parent t)
    pr_ch = take n (children t)
    pruned_tree = TR pr_label pr_parent pr_ch n

create_trees :: Tree -> [Transaction] -> Mode -> [Tree]
create_trees t [] _ = [t]
create_trees t (tx:txs) mode =
  foldl (++) [] (map (\tr -> create_trees tr txs mode) list_trees)
  where
    parent_addr = source tx
    pot_dad = search_dads t mode
    match_dad = filter (\v -> ((label t) !! v) == parent_addr) pot_dad
    list_trees = map (\v -> add_node t (dest tx) v) match_dad 

gen_all_trees :: [Transaction] -> Mode -> [Tree]
gen_all_trees [] _ = []
gen_all_trees tx mode =
  create_trees (new_tree $ source (head tx)) tx mode

list_tree_size :: [Tree] -> Int
list_tree_size t = length t

dfs :: Vertex -> Int -> Tree -> Map.Map Address Int -> (Bool, Map.Map Address Int)
dfs vertex level t deepest_level =
  if l > level then
    (False, deepest_level)
  else
   foldl (\(b,m) c -> (if b then dfs c (level + 1) t m else (False, m))) (True, updated_d_l) children_vertex 
  where
    address = (label t) !! vertex
    l = Map.findWithDefault 0 address deepest_level
    updated_d_l = Map.insert address level deepest_level
    children_vertex = (children t) !! vertex

check_equal_exec :: Tree -> Bool
check_equal_exec t =
  fst (dfs 0 1 t Map.empty)

are_equal :: [Transaction] -> Mode -> Bool
are_equal tx mode = 
  foldl (&&) True list_eq
  where
    all_trees = gen_all_trees tx mode
    list_eq = map check_equal_exec all_trees

are_equal_ :: [Transaction] -> Mode -> [Bool]
are_equal_ tx mode = 
  list_eq
  where
    all_trees = gen_all_trees tx mode
    list_eq = map check_equal_exec all_trees


typesTB :: TheoryBuilder
typesTB =
  typeSynonym "Vertex" (Proxy @Vertex)
  >> typeSynonym "Address" (Proxy @Address)
  >> typeSynonym "Label" (Proxy @Label)
  >> typeSynonym "Parent" (Proxy @Parent)
  >> typeSynonym "Children" (Proxy @Children)
  >> addType (Proxy @Mode)
  >> addType (Proxy @Transaction)
  >> addType (Proxy @Tree)

funsTB :: TheoryBuilder
funsTB = do
  addSymbol "TX" $ totheory TX
  addSymbol "gen_all_trees" $ totheory gen_all_trees
  addSymbol "are_equal" $ totheory are_equal

tzexectheory :: TheoryBuilder
tzexectheory = 
  typesTB
  >> funsTB
