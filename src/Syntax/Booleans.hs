{-#LANGUAGE FlexibleInstances#-}
{-#LANGUAGE UndecidableInstances#-}
module Syntax.Booleans where

import Syntax.HLPrelude
import qualified Prelude as P

import Lola

myor :: Maybe Bool -> Maybe Bool -> Maybe Bool
myor (Just True) _ = Just True
myor _ (Just True) = Just True
myor (Just x) (Just y) = Just (x P.|| y)
myor _ _ = Nothing

myand :: Maybe Bool -> Maybe Bool -> Maybe Bool
myand (Just False) _ = Just False
myand _ (Just False) = Just False
myand (Just x) (Just y) = Just (x P.&& y)
myand _ _ = Nothing

lfor = getsmp2 myor
lfand = getsmp2 myand

infixr 2 ||
(||) :: Expr Bool -> Expr Bool -> Expr Bool
-- a || b = (P.||) <$> a <*> b
a || b = lfor <$> a <*> b

infixr 3 &&
(&&) :: Expr Bool -> Expr Bool -> Expr Bool
a && b = lfand <$> a <*> b

not :: Expr Bool -> Expr Bool
not a = P.not <$> a

infixr 2 `implies`
implies :: Expr Bool -> Expr Bool -> Expr Bool
implies a b = not a || b

ite :: Maybe Bool -> Maybe a -> Maybe a -> Maybe a
ite (Just True) x _ = x
ite (Just False) _ y = y
ite Nothing _ _ = Nothing

ifThenElse :: Streamable a => Expr Bool -> Expr a -> Expr a -> Expr a
ifThenElse b t e = itelfun <$> b <*> t <*> e

itelfun :: Streamable a => LFunction Bool (LFunction a (LFunction a a))
itelfun = getsmp3 ite

infixr 4 ===
(===) :: (Eq a, Streamable a) => Expr a -> Expr a -> Expr Bool
a === b = (P.==) <$> a <*> b

infixr 4 /==
(/==) :: (Eq a, Streamable a) => Expr a -> Expr a -> Expr Bool
a /== b = (P./=) <$> a <*> b
