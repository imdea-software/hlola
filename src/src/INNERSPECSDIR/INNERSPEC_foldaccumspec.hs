{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
module INNERSPECSDIR.INNERSPEC_foldaccumspec where
import Lola
import GHC.Generics
import Data.Aeson
import Syntax.HLPrelude
import DecDyn (InnerSpecification(IS), bind)
import Syntax.Booleans
import Syntax.Ord
import Syntax.Num
import qualified Prelude as P
import Lib.Utils
import Data.Dynamic


-- Custom Haskell


foldaccumspec :: forall a.(Typeable a, Show a, ToJSON a, FromJSON a, Eq a, Read a) =>  (a->a->a) -> (a->a->a) -> Maybe a -> [a] -> [a] -> InnerSpecification a
foldaccumspec op1 op2 mabs phi__arg psi__arg = IS [bind phi phi__arg, bind psi psi__arg] ret stop (2)
  where

  phi :: Stream a
  phi = Input "phi"
  psi :: Stream a
  psi = Input "psi"

  accum_phi :: Stream a
  accum_phi = "accum_phi" =: (if  (instantN:@(0, Leaf undefined))  === 1 then  (phi:@(0, Leaf undefined))  else (magic2 op2)  (accum_phi:@(-1,(toolLift undefined)))   (phi:@(0, Leaf undefined)))

  ret :: Stream a
  ret = "ret" =: (let
    val = (magic2 op2)  (psi:@(0, Leaf undefined))   (accum_phi:@(0, Leaf undefined)) 
    in if (instantN:@(0, Leaf undefined)) === 1 then val else (magic2 op1) (ret:@(-1, Leaf undefined))  val)

  stop :: Stream Bool
  stop = "stop" =: (maybe ((toolLift False)) (\abs ->  (ret:@(0, Leaf undefined))  === (toolLift abs)) mabs)
