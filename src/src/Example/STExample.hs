{-# Language RebindableSyntax #-}
module Example.STExample where

import Lola
import Syntax.HLPrelude
import Syntax.Booleans
import Syntax.Num()
import DecDyn
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Prelude as P ((<), (>), (<=), (>=), (&&))
import Lib.MTL (eventually)
import Theories.Dijkstra

-- type Node = String
-- type Weight = Double
-- type Graph = Map.Map Node (Set.Set (Node, Weight))
-- SpatioTemporal Formula
type Formula a = Graph -> Node -> Stream a

data NodeType = BusStop | Hospital | MetroStop | MainSquare | Museum deriving Eq

neighbours :: Graph -> Node -> Set.Set (Node, Weight)
neighbours = (Map.!)

-- I don't think it works for paths with negative weights

reaches :: (Weight, Weight) -> Formula Bool -> Formula Bool -> Formula Bool
reaches (n,m) phi psi gr no = "reaches" <: (n,m) <: phi gr no <: psi gr no <: no =: reaches' n m no
  where
  reaches' n' m' no'
    | m P.< 0 = Leaf False
    | n P.> 0 = checkneighs n' m' no'
    | otherwise = Now (psi gr no') || checkneighs n' m' no'
  checkneighs n' m' no' = Now (phi gr no') && foldl  (\acc (no'', w) -> acc || Now (reaches (n'-w, m'-w) phi psi gr no'')) (Leaf False) (neighbours gr no')

escapes :: (Weight, Weight) -> Formula Bool -> Formula Bool
escapes (n,m) phi gr no = "escapes" <: (n,m) <: phi gr no <: no =:
  escapes' Set.empty no
  where
    distances = dijkstra no gr
    escapes' visited no'
      | mydist P.<= m P.&& mydist P.>= n = Now $ phi gr no'
      | otherwise = (Now $ phi gr no') && Set.foldl (\acc no'' -> acc || escapes' nextvisited no'') (Leaf False) unvisitedneighs
      where
      mydist = distances Map.! no'
      myneighs = Set.map fst $ neighbours gr no'
      unvisitedneighs = Set.difference myneighs visited
      nextvisited = Set.insert no' visited

-- -=-=-=-=-=-=-=-=-=-=-=

f :: Formula Bool
f graph node = let
  neighs = neighbours graph node
  in
  "formf" =: foldl (\acc (neigh,_) -> acc || Now (f' graph neigh)) (Leaf False) neighs

f' :: Formula Bool
f' _ "p" = "f'p" =: Leaf True
f' _ "r" = "f'r" =: Leaf True
f' _ n = "f'" ++ n =: Leaf False

mygraph = Map.fromList [("p", (Set.singleton ("r",10))), ("r", (Set.singleton ("s", 3))), ("s", (Set.singleton ("p", 8)))]

x :: Stream Bool
-- x = reaches 0 (\_ _ -> "trueForm" =: Leaf True) f mygraph "p"
x = propescape

examplecity = Map.fromList [
        ("0", Set.fromList [("1",2), ("5",2)]),
        ("1", Set.fromList [("0",2), ("2",9), ("6",4)]),
        ("2", Set.fromList [("1",9), ("3",3)]),
        ("3", Set.fromList [("2",3), ("4",6), ("6",15)]),
        ("4", Set.fromList [("3",6), ("5",7)]),
        ("5", Set.fromList [("4",7)]),
        ("6", Set.fromList [("1",4), ("3",15)])
        ]

nodeType :: Formula NodeType
nodeType _ node = "nodeType" <: node =: case node of
  "0" -> Leaf BusStop
  "1" -> Leaf Hospital
  "2" -> Leaf MetroStop
  "3" -> Leaf MainSquare
  "4" -> Leaf BusStop
  "5" -> Leaf Museum
  "6" -> Leaf MetroStop

taxiAvail :: Formula Bool
taxiAvail _ node = "taxiAvail" <: node =: case node of
  "0" -> Leaf False
  "1" -> Leaf False
  "2" -> Leaf True
  "3" -> Leaf False
  "4" -> Leaf False
  "5" -> Leaf True
  "6" -> Leaf False

peopleAt :: Formula Int
peopleAt _ node = "peopleAt" <: node =: case node of
  "0" -> Leaf 3
  "1" -> Leaf 145
  "2" -> Leaf 67
  "3" -> Leaf 243
  "4" -> Leaf 22
  "5" -> Leaf 103
  "6" -> Leaf 6

prop1 :: Stream Bool
prop1 = taxiAvail examplecity "0"

prop2 :: Stream Bool
prop2 = eventually (0,2) (taxiAvail examplecity "0")

prop3 :: Stream Bool
prop3 = "prop3" =: not (Now $ taxiAvail examplecity "2") ||  Now (taxiReachStop 5 10 examplecity "2")

taxiReachStop :: Weight -> Weight -> Formula Bool
taxiReachStop from to g n = reaches (from,to) taxiAvail stopReachMainSquare g n

stopReachMainSquare :: Formula Bool
stopReachMainSquare g n = reaches (0,10) isThereAStop isMainSquare g n

isMainSquare :: Formula Bool
isMainSquare g n = "isManSquare" <: g <: n =: Now (nodeType g n) === Leaf MainSquare
isThereAStop :: Formula Bool
isThereAStop g n = "isThereAStop" <: g <: n =: Now (nodeType g n) === Leaf BusStop || Now (nodeType g n) === Leaf MetroStop

propescape :: Stream Bool
propescape = escapes (1,10) taxiAvail examplecity "2"
