{-# LANGUAGE ScopedTypeVariables #-}
module Theories.Dijkstra where
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Control.Monad.ST
import Control.Monad (foldM)
import qualified Data.Set as S
import qualified Data.Map.Strict as Map

type Node = String
type Weight = Double
type Graph = Map.Map Node (S.Set (Node, Weight))


dijkstra :: Node -> Graph -> Map.Map Node Weight
dijkstra src graph = let
  arr = dijkstra' src graph
  list = zip (Map.keys graph) (elems arr)
  in
  Map.fromList list

{- Adapted from -}
{- https://rosettacode.org/wiki/Dijkstra%27s_algorithm#Haskell -}

dijkstra' :: Node -> Graph -> Array Int Weight
dijkstra' src graph = runST $ do
  let graphsize = Map.size graph
  min_distance <- newSTArray (0, graphsize) 9999999999999
  writeArray min_distance (Map.findIndex src graph) 0
  let aux (vertex_queue :: S.Set (Weight, Node)) =
        case S.minView vertex_queue of
          Nothing -> return ()
          Just ((dist, u), vertex_queue') ->
            let edges = graph Map.! u
                f vertex_queue (v, weight) = do
                  let dist_thru_u = dist + weight
                  old_dist <- readArray min_distance (Map.findIndex v graph)
                  if dist_thru_u >= old_dist then
                    return vertex_queue
                  else do
                    let vertex_queue' = S.delete (old_dist, v) vertex_queue
                    writeArray min_distance (Map.findIndex v graph) dist_thru_u
                    return $ S.insert (dist_thru_u, v) vertex_queue'
            in
            foldM f vertex_queue' edges >>= aux
  aux (S.singleton (0, src))
  m <- freeze min_distance
  return m
  where
        newSTArray :: Ix i => (i,i) -> e -> ST s (STArray s i e)
        newSTArray = newArray

shortest_path_to :: (Ix v) => v -> v -> Array v v -> [v]
shortest_path_to target invalid_index previous =
  aux target [] where
    aux vertex acc | vertex == invalid_index = acc
                   | otherwise = aux (previous ! vertex) (vertex : acc)

adj_list :: Array Char [(Char, Int)]
adj_list = listArray ('a', 'f') [ [('b',7), ('c',9), ('f',14)],
                                  [('a',7), ('c',10), ('d',15)],
                                  [('a',9), ('b',10), ('d',11), ('f',2)],
                                  [('b',15), ('c',11), ('e',6)],
                                  [('d',6), ('f',9)],
                                  [('a',14), ('c',2), ('e',9)] ]

-- main :: IO ()
-- main = do
--   let (min_distance, previous) = dijkstra 'a' ' ' adj_list
--   putStrLn $ "Distance from a to e: " ++ show (min_distance ! 'e')
--   let path = shortest_path_to 'e' ' ' previous
--   putStrLn $ "Path: " ++ show path
