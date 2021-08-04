module Syntax.Num where

import Syntax.HLPrelude
import qualified Prelude as P

import Lola

mymul :: (Streamable a, Num a, Eq a) => Maybe a -> Maybe a -> Maybe a
mymul (Just 0) _ = Just 0
mymul _ (Just 0) = Just 0
mymul (Just x) (Just y) = Just (x*y)
mymul _ _ = Nothing

-- Pretty writing
-- Infix notation for + and *
instance (Streamable a, Num a, Eq a) => Num (Expr a) where
  x + y = (+) <$> x <*> y
  x * y = getsmp2 mymul <$> x <*> y
  fromInteger = Leaf . fromInteger
  abs = App (Leaf abs)
  signum = App (Leaf signum)
  negate = App (Leaf negate)

-- TODO This can be simplified
mod :: (Streamable a,Integral a) => Expr a -> Expr a -> Expr a
mod a b = P.mod <$> a <*> b

-- TODO This can be simplified
infix 7 /
(/) :: (Streamable a,Fractional a) => Expr a -> Expr a -> Expr a
a / b = (P./) <$> a <*> b

-- TODO This can be simplified
intdiv :: Expr Int -> Expr Int -> Expr Double
intdiv n m = (fromIntegral <$> n) / (fromIntegral <$> m)

instance (Streamable a, Fractional a, Eq a) => Fractional (Expr a) where
  fromRational = Leaf . fromRational
  recip = App (Leaf recip)
