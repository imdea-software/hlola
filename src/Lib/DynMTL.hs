module Lib.DynMTL where

import Lola
import Syntax.HLPrelude
import Syntax.Booleans
import Syntax.Ord
import Syntax.Num()
import DecDyn
import Engine.Engine (runSpec)
import Data.Dynamic
import qualified Prelude as P ((<))

-- phi U_{a,b} psi
until :: (Int, Expr Int) -> Stream Bool -> Stream Bool -> Stream Bool
until (a, eb) phi psi = "until" <: (a,eb) <: phi <: psi =: untilaux :@ (a, if a P.< 0 then Now $ until (0, eb + Leaf a) phi psi else Leaf undefined)
  where
  untilaux = "untilaux" <: (a,eb) <: phi <: psi =: untilexpr
  winphis = phi :@@ (eb - Leaf a)
  winpsis = psi :@@ (eb - Leaf a)
  untilexpr = runSpec <$> (untilspec <$> winphis <*> winpsis)

untilspec phis psis = IS [bind phi phis, bind psi psis] psi stop 2
  where
  psi = Input "psi"
  phi = Input "phi"
  stop = "stop" =: Now psi || not (Now phi)

eventually :: (Int, Int) -> Stream Bool -> Stream Bool
eventually (a,b) phi = "eventually" <: a <: b <: phi =: eventuallyaux :@ (a, Leaf False)
  where
  eventuallyaux = "eventuallyaux" <: a <: b <: phi =: or <$> phi :@@ (Leaf (b-a))

always :: (Int, Int) -> Stream Bool -> Stream Bool
always (a,b) phi = "always" <: a <: b <: phi =: alwaysaux :@ (a, Leaf False)
  where
  alwaysaux = "alwaysaux" <: a <: b <: phi =: or <$> phi :@@ (Leaf (b-a))
