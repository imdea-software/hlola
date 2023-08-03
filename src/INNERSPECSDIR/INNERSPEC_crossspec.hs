{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
module INNERSPECSDIR.INNERSPEC_crossspec where
import Lola
import GHC.Generics
import Data.Aeson
import Syntax.HLPrelude
import DecDyn (InnerSpecification(), createIS, bind)
import Syntax.Booleans
import Syntax.Ord
import Syntax.Num
import qualified Prelude as P


-- Custom datas


-- Custom Haskell


crossspec ::   [Double] -> [Double] -> InnerSpecification Bool
crossspec  r__arg s__arg = createIS [bind r r__arg, bind s s__arg] cross cross (1)
  where

  r :: Stream Double
  r = Input "r"
  s :: Stream Double
  s = Input "s"

  cross :: Stream Bool
  cross = "cross" =: (
    (magic1 signum) ( (r:@(0, Leaf undefined))  -  (s:@(0, Leaf undefined)) ) /== (magic1 signum) ( (r:@(-1, (r:@(0, Leaf undefined)) ))  -  (s:@(-1, (s:@(0, Leaf undefined)) )) ))