{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Lib.QMTL where
import Lola
import Syntax.HLPrelude
import Syntax.Booleans
import Syntax.Ord
import Syntax.Num
import qualified Prelude as P
import Lib.Utils
import INNERSPECSDIR.INNERSPEC_foldspec
import INNERSPECSDIR.INNERSPEC_foldaccumspec
import Data.Dynamic
import Data.Maybe
import Lib.Lola
import Theories.Lattice
import Data.Aeson


-- Custom Haskell

-- Constants


previously :: (Lattice a, Typeable a, Show a, ToJSON a, FromJSON a, Eq a, Read a) => (Int, Int) -> Stream a -> Stream a
previously (a,b) phi = "previously" <: (a,b) <: phi =: (let
  win = slidingwin (a,b) phi in
  if (toolLift null) win then (toolLift (fromJust mabscap))  else (magic1 runSpec) ((magic1 (foldspec sq_cup mabscup))  win))

historically :: (Lattice a, Typeable a, Show a, ToJSON a, FromJSON a, Eq a, Read a) => (Int, Int) -> Stream a -> Stream a
historically (a,b) phi = "historically" <: (a,b) <: phi =: (let
  win = slidingwin (a,b) phi in
  if (toolLift null) win then (toolLift (fromJust mabscup))  else (magic1 runSpec) ((magic1 (foldspec sq_cap mabscap))  win))

since :: (ToJSON a, FromJSON a, Read a, Show a, Eq a, Typeable a, Lattice a) => (Int, Int) -> Stream a -> Stream a -> Stream a
since (a,b) phi psi = "since" <: (a,b) <: phi <: psi =: (let
  phis = slidingwin (a,b) phi
  psis = slidingwin (a,b) psi
  in (magic1 runSpec) ((magic2 (foldaccumspec sq_cup sq_cap mabscup))  phis psis))

since_overline :: (ToJSON a, FromJSON a, Read a, Show a, Eq a, Typeable a, Lattice a) => (Int, Int) -> Stream a -> Stream a -> Stream a
since_overline (a,b) phi psi = "since_overline" <: (a,b) <: phi <: psi =: (let
  phis = slidingwin (a,b) phi
  psis = slidingwin (a,b) psi
  in (magic1 runSpec) ((magic2 (foldaccumspec sq_cap sq_cup mabscap))  phis psis))
