{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
module INNERSPECSDIR.INNERSPEC_foldspec where
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


foldspec :: forall a.(Typeable a, Show a, ToJSON a, FromJSON a, Eq a, Read a) =>  (a->a->a) -> Maybe a -> [a] -> InnerSpecification a
foldspec op mabs vals__arg = IS [bind vals vals__arg] ret stop (2)
  where

  vals :: Stream a
  vals = Input "vals"

  ret :: Stream a
  ret = "ret" =: (if  (instantN:@(0, Leaf undefined))  === 1 then  (vals:@(0, Leaf undefined))  else (magic2 op)  (ret:@(-1,(toolLift undefined)))   (vals:@(0, Leaf undefined)))

  stop :: Stream Bool
  stop = "stop" =: (maybe ((toolLift False)) (\abs ->  (ret:@(0, Leaf undefined))  === (toolLift abs)) mabs)
