module Syntax.Ord where

import Syntax.HLPrelude
import qualified Prelude as P

import Lola

infix 4 <
(<) :: (Ord a, Streamable a) => Expr a -> Expr a -> Expr Bool
a < b = (P.<) <$> a <*> b

infix 4 >
(>) :: (Ord a, Streamable a) => Expr a -> Expr a -> Expr Bool
a > b = (P.>) <$> a <*> b

infix 4 <=
(<=) :: (Ord a, Streamable a) => Expr a -> Expr a -> Expr Bool
a <= b = (P.<=) <$> a <*> b

infix 4 >=
(>=) :: (Ord a, Streamable a) => Expr a -> Expr a -> Expr Bool
a >= b = (P.>=) <$> a <*> b
