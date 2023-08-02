module Theories.CoverStrategies.LRTA where

import Data.Array
import Data.Function
import Data.List
import Theories.CoverStrategies.Types

go :: Position -> Matrix -> (Position, Matrix)
go pos@(px,py) mat = let
  neighs = [(px-1,py), (px+1,py), (px, py-1), (px, py+1)]
  ((minx,miny), (maxx, maxy)) = bounds mat
  validneighs = filter (\(x,y) -> within x minx maxx && within y miny maxy) neighs
  within a b c = b <= a && a <= c
  ixvals = map (\ix -> (ix, mat ! ix)) validneighs
  validixvals = filter ((>=0).snd) ixvals
  (minix, minval) = minimumBy (compare `on` snd) validixvals
  newmatrix = mat // [(pos, minval + 1)] -- if it is not -1
  in (minix, newmatrix)
