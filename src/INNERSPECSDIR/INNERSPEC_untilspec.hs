{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module INNERSPECSDIR.INNERSPEC_untilspec where
import Lola
import GHC.Generics
import Data.Aeson
import Syntax.HLPrelude
import DecDyn (InnerSpecification(), createIS, bind)
import Syntax.Booleans
import Syntax.Ord
import Syntax.Num
import qualified Prelude as P


-- Custom Haskell


untilspec ::  [Bool] -> [Bool] -> InnerSpecification Bool
untilspec  phi__arg psi__arg = createIS [bind phi phi__arg, bind psi psi__arg] psi stop 2
  where

  phi :: Stream Bool
  phi = Input "phi"
  psi :: Stream Bool
  psi = Input "psi"

  stop :: Stream Bool
  stop = "stop" =: (psi:@(0, Leaf undefined) || not (phi:@(0, Leaf undefined)))
