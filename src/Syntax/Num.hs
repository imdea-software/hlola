module Syntax.Num where

import Syntax.HLPrelude
import qualified Prelude as P

import Lola

-- Pretty writing
-- Infix notation for + and *
instance (Streamable a, Num a) => Num (Expr a) where
  x + y = (+) <$> x <*> y
  x * y = (*) <$> x <*> y
  fromInteger = Leaf . fromInteger
  abs = App (Leaf abs)
  signum = App (Leaf signum)
  negate = App (Leaf negate)

mod :: (Streamable a,Integral a) => Expr a -> Expr a -> Expr a
mod a b = P.mod <$> a <*> b

infix 7 /
(/) :: (Streamable a,Fractional a) => Expr a -> Expr a -> Expr a
a / b = (P./) <$> a <*> b

intdivide :: Expr Int -> Expr Int -> Expr Double
intdivide n m = (fromIntegral <$> n) / (fromIntegral <$> m)
