{-#LANGUAGE FlexibleInstances#-}
{-#LANGUAGE UndecidableInstances#-}
module Syntax.Booleans where

import Syntax.HLPrelude
import qualified Prelude as P

import Lola

infixr 2 ||
(||) :: Expr Bool -> Expr Bool -> Expr Bool
a || b = (P.||) <$> a <*> b

infixr 3 &&
(&&) :: Expr Bool -> Expr Bool -> Expr Bool
a && b = (P.&&) <$> a <*> b

not :: Expr Bool -> Expr Bool
not a = P.not <$> a

implies :: Expr Bool -> Expr Bool -> Expr Bool
implies a b = not a || b

ite :: Bool -> a -> a -> a
ite True x _ = x
ite False _ y = y

ifThenElse :: Streamable a => Expr Bool -> Expr a -> Expr a -> Expr a
ifThenElse b t e = ite <$> b <*> t <*> e

infixr 4 ===
(===) :: (Eq a, Streamable a) => Expr a -> Expr a -> Expr Bool
a === b = (P.==) <$> a <*> b

infixr 4 /==
(/==) :: (Eq a, Streamable a) => Expr a -> Expr a -> Expr Bool
a /== b = (P./=) <$> a <*> b
