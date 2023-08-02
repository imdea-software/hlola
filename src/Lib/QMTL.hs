{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Lib.QMTL (eventually, always, since, since') where
import Lola
import Syntax.HLPrelude
import Syntax.Booleans
import Syntax.Ord
import Syntax.Num
import qualified Prelude as P
import Theories.Lola
import Lib.Utils
import INNERSPECSDIR.INNERSPEC_foldspec
import INNERSPECSDIR.INNERSPEC_foldaccumspec
import Theories.Lattice
import Data.Dynamic
import Data.Maybe
import Theories.Lattice
import Data.Aeson


-- Custom datas


-- Custom Haskell

-- Constants


eventually :: (Lattice a, Typeable a, Show a, ToJSON a, FromJSON a, Eq a, Read a) => (Int, Int) -> Stream a -> Stream a
eventually (x,y) phi = "eventually" <: (x,y) <: phi =: (let
  win = slidingwin (x,y) phi in
  if (magic1 null) win then toolLift (fromJust opt_bottom) else (magic1 runSpec) ((magic1 (foldspec sqcup opt_top))  win))

always :: (Lattice a, Typeable a, Show a, ToJSON a, FromJSON a, Eq a, Read a) => (Int, Int) -> Stream a -> Stream a
always (x,y) phi = "always" <: (x,y) <: phi =: (let
  win = slidingwin (x,y) phi in
  if (magic1 null) win then toolLift (fromJust opt_top) else (magic1 runSpec) ((magic1 (foldspec sqcap opt_bottom))  win))

since :: (ToJSON a, FromJSON a, Read a, Show a, Eq a, Typeable a, Lattice a) => (Int, Int) -> Stream a -> Stream a -> Stream a
since (x,y) phi psi = "since" <: (x,y) <: phi <: psi =: (let
  phis = slidingwin (x,y) phi
  psis = slidingwin (x,y) psi
  in (magic1 runSpec) ((magic2 (foldaccumspec sqcup sqcap opt_top))  phis psis))

since' :: (ToJSON a, FromJSON a, Read a, Show a, Eq a, Typeable a, Lattice a) => (Int, Int) -> Stream a -> Stream a -> Stream a
since' (x,y) phi psi = "since'" <: (x,y) <: phi <: psi =: (let
  phis = slidingwin (x,y) phi
  psis = slidingwin (x,y) psi
  in (magic1 runSpec) ((magic2 (foldaccumspec sqcap sqcup opt_bottom))  phis psis))
