{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Lola where

import           Prelude

import           Data.Typeable
import           Data.Aeson

type Ident = String
type Streamable = Typeable

-- Data types for simplifiers
data LFunction a b = Pure (a->b) | Simplifier (Maybe a -> Maybe b)

class ILFunction x where
  toLFunction :: x a b -> LFunction a b

instance ILFunction (->) where
  toLFunction = Pure

instance ILFunction LFunction where
  toLFunction = id

getMayber :: LFunction a b -> Maybe a -> Maybe b
getMayber (Simplifier f) x = f x
getMayber (Pure _) Nothing = Nothing
getMayber (Pure g) (Just x) = Just (g x)

-- Public for simplifiers
getsmp1 :: (Maybe a -> Maybe b) -> LFunction a b
getsmp1 = Simplifier

getsmp2 :: (Maybe a-> Maybe b -> Maybe c) -> LFunction a (LFunction b c)
getsmp2 f = Simplifier $ Just .getsmp1.f

getsmp3 :: (Maybe a-> Maybe b -> Maybe c -> Maybe d) -> LFunction a (LFunction b (LFunction c d))
getsmp3 f = Simplifier $ Just .getsmp2.f
--

-- | @Expr@ represents our most basic /AST/. Which is basically a [Free
-- Applicative Functor](https://arxiv.org/abs/1403.0749)
-- plus some kind of projection @(:@)@
data Expr a where
  -- | An element of type @a@
  Leaf :: Streamable a => a -> Expr a
  -- | Function application, sadly this constructor hides @b :: *@
  App  :: (Streamable a, Streamable b, ILFunction f, Streamable (f b a)) => Expr (f b a) -> Expr b -> Expr a
  ----------------------------------------
  -- | Now s = s :@ (0,_)
  -- @Now@ gets (projects) the value of a stream /now/
  Now :: Declaration a -> Expr a
  -- | Projection Constructor:
  (:@) :: Declaration a -- ^ For a given @e :: Expr a@
       -> (Int, Expr a) -- ^ an offset @i@ and default @c@
       -> Expr a -- ^ gives us back @id[i | c]@

infixl 4 <$>
(<$>) :: (Streamable a, Streamable b, ILFunction f, Streamable (f a b)) => f a b -> Expr a -> Expr b
f <$> e = App (Leaf f) e

infixl 4 <*>
(<*>) :: (Streamable a, Streamable b, ILFunction f, Streamable (f a b)) => Expr (f a b) -> Expr a -> Expr b
e0 <*> e1 = App e0 e1

instance Show (Expr a) where
  show (Leaf _)       = "Leaf"
  show (App e1 e2)    = "App (" ++ show e1 ++ ", " ++ show e2 ++ ")"
  show (Now dec)      = "Now " ++ show (getId dec)
  show (dec :@ (i,_)) = show (getId dec) ++ "[" ++ show i ++ "|default]"

instance Show (Declaration a) where
  show (Input ident)        = ident
  show (Output (ident, _)) = ident

-- | @Declaration@ represents a single variable __typed__ declaration
-- /s_n = expr/. Note here that @Declaration@ is hiding @a@ which is not always
-- desirable and might be troublesome
data Declaration a where
  Input :: (FromJSON a, Read a, Streamable a) => Ident -> Declaration a
  Output :: Streamable a => (Ident, Expr a) -> Declaration a

getId :: Declaration a -> Ident
getId (Input x)     = x
getId (Output (x,_)) = x

----------------------------------------
-- Smart Constructors
----------------------------------------
-- These are useful to build a Lola System
-- where:
-- s1 = e1(...)
-- s2 = e2(...)
-- ...
-- sn = en(...)
-- becomes something like
-- s1 =: e1
-- s2 =: e2 ....
-- or s1 =:= e1 ...

infix 1 =:
(=:) :: Streamable a => Ident -> Expr a -> Declaration a
s =: e = Output (s , e)

infix 1 =:=
(=:=) :: Streamable a => Ident -> a -> Declaration a
s =:= e = Output (s , Leaf e)

infixl 2 <:
(<:) :: Show a => Ident -> a -> Ident
ident <: decName = ident ++ ('<':show decName ++ ">")

type Stream = Declaration
