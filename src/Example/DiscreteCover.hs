{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.DiscreteCover where

import Lola
import Syntax.HLPrelude
import DecDyn
import Syntax.Booleans
import Lib.Utils
import Theories.Geometry2D
import Data.Maybe
import Data.List
import Data.Array
import Data.Function
import GHC.Generics
import qualified Prelude as P
import Theories.CoverStrategies.Types

spec :: (Position -> Matrix -> (Position, Matrix)) -> Int -> Specification
spec go minvisits = [out goto, out mission_finished]
  where

  evs_within :: Stream [String]
  evs_within = Input "events_within"

  inimatrix :: Stream (Position, String)
  inimatrix = Input "inimatrix"

  position :: Stream Position
  position = Input "position"

  reqmove :: Stream Bool
  reqmove = "reqmove" =: elem "go.next" <$> Now evs_within

  matrix :: Stream Matrix
  matrix = "matrix" =:
    if (/=0).snd.fst <$> Now inimatrix then creatematrix <$> Now inimatrix
    else snd <$> retmatrix :@(-1, Leaf (undefined, dfltmatrix))

  retmatrix :: Stream (Maybe Position, Matrix)
  retmatrix = "retmatrix" =:
    if Now reqmove then mapFst Just <$> (go <$> Now position <*> Now matrix)
    else ((,) Nothing) <$> Now matrix
    where
    mapFst f (a, b) = (f a, b)

  goto :: Stream (Maybe Position)
  goto = "goto" =: fst <$> Now retmatrix

  dbgmatrix :: Stream [Int]
  dbgmatrix = "dbgmatrix" =: elems <$> Now matrix

  countermatrix :: Stream Matrix
  countermatrix = "countermatrix" =: let
    prevmatrix = countermatrix :@ (-1, Leaf dfltmatrix)
    pos = Now goto
    val = fmap <$> ((!) <$> prevmatrix) <*> pos
    in
    if (/=0).snd.fst <$> Now inimatrix then creatematrix <$> Now inimatrix
    else if isJust <$> Now goto
    then update <$> prevmatrix <*> pos <*> val
    else prevmatrix
    where
    update m Nothing Nothing = m
    update m (Just p) (Just v) = m // [(p,v+1)]

  mission_finished :: Stream Bool
  mission_finished = "mission_finished" =:
    if isNothing <$> Now goto || Now mission_already_finished then Leaf False else
    checkDone <$> countermatrix :@ (-1,Leaf dfltmatrix)
    where checkDone m = all (\x -> x == -1 P.|| x P.>= minvisits) (elems m)

  mission_already_finished :: Stream Bool
  mission_already_finished = "maf" =:
    mission_already_finished :@ (-1,Leaf False) || 
    mission_finished :@ (-1,Leaf False)
