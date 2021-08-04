{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
module INNERSPECSDIR.INNERSPEC_headismax where
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


headismax ::   [Double] -> InnerSpecification Bool
headismax  vals__arg = IS [bind vals vals__arg] ret stop (2)
  where

  vals :: Stream Double
  vals = Input "vals"

  head :: Stream Double
  head = "head" =: ((head:@(-1, (vals:@(0, Leaf undefined)) )))

  ret :: Stream Bool
  ret = "ret" =: ((head:@(0, Leaf undefined))  >  (vals:@(0, Leaf undefined)))

  stop :: Stream Bool
  stop = "stop" =: (not  (ret:@(0, Leaf undefined)))
