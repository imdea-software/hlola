{-# LANGUAGE RebindableSyntax  #-}
module Example.SWSEx where

import Lola
import Syntax.HLPrelude
import Syntax.Num
import Syntax.Ord
import DecDyn
import Data.Map.Strict (empty)
import Engine.Engine (runSpec)
import qualified Data.Map.Strict as Map (empty)

spec :: Specification
spec = [out innerfibo]

innerfibo :: Stream Int
innerfibo = "innerfibo" =: Leaf (runSpec dainner)

dainner :: InnerSpecification Int
dainner = IS (repeat Map.empty) fibo stopy 3

fibo :: Stream Int
fibo = "fibo" =: fibo :@ (-1,1) + fibo :@ (-2,0)

stopy :: Stream Bool
stopy = "stopy" =: Now fibo > 22
