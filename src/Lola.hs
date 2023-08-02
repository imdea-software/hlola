{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}

module Lola where

import           Prelude hiding ((<$>), (<*>))

import           Data.Typeable
import           Data.Aeson
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Dynamic
import Data.Default

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
getMayber (Simplifier f) x = let
  -- !res = f x -- Activate this to always enforce strict evaluation of arguments
  res = f x
  in res
getMayber (Pure _) Nothing = Nothing
getMayber (Pure g) (Just x) = let
  -- !res = g x -- Activate this to always enforce strict evaluation of arguments
  res = g x
  in Just res

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
  (:@@) :: Streamable a => Declaration a -> Expr Int -> Expr [a] -- Slice of size n
  Over :: (Streamable a, Show a, ToJSON a) =>
    (x->Stream a) ->
    Stream (Set.Set x) ->
    Initer x a ->
    (Maybe (Expr (Set.Set x))) ->
    (Expr (Map.Map x a) -> Expr (Map.Map x a)) ->
    Expr (Map.Map x a)
  MOver :: (Default x, ToJSON x, Show x, Ord x, Streamable x, Streamable a, Show a, ToJSON a) =>
    (x->Stream a) ->
    Stream (Maybe x) ->
    Initer x a ->
    (Maybe (Expr (Set.Set x))) ->
    (Expr (Maybe a) -> Expr (Maybe a)) ->
    Expr (Maybe a)

type Initer x a = Expr ((x -> Stream a) -> [Map.Map Ident Value] -> x -> ([Map.Map Ident Dynamic], Maybe a))

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

class ToolLift r f where
  toolLift :: f -> r

instance {-# OVERLAPS #-} (a ~ a', Streamable a) => ToolLift (Expr a') a where
  toolLift = magic0

instance {-# OVERLAPS #-} (a ~ a', b~b', Streamable a, Streamable b) => ToolLift (Expr a' -> Expr b') (a->b) where
  toolLift = magic1

instance {-# OVERLAPS #-} (a ~ a', b~b', c~c', Streamable a, Streamable b, Streamable c) => ToolLift (Expr a' -> Expr b' -> Expr c') (a->b->c) where
  toolLift = magic2

instance {-# OVERLAPS #-} (a ~ a', b~b', c~c', d~d', Streamable a, Streamable b, Streamable c, Streamable d) => ToolLift (Expr a' -> Expr b' -> Expr c' -> Expr d') (a->b->c->d) where
  toolLift = magic3

instance {-# OVERLAPS #-} (a ~ a', b~b', c~c', d~d', a0~a0', a1~a1', a2~a2', a3~a3', Streamable a, Streamable b, Streamable c, Streamable d, Streamable a0, Streamable a1, Streamable a2, Streamable a3) => ToolLift (Expr a' -> Expr b' -> Expr c' -> Expr d' -> Expr a0 -> Expr a1 -> Expr a2 -> Expr a3) (a->b->c->d->a0->a1->a2->a3) where
  toolLift = magic7

magic0 f = Leaf f
magic1 f x = f Lola.<$> x
magic2 f x y = f Lola.<$> x Lola.<*> y
magic3 f x y z = f Lola.<$> x Lola.<*> y Lola.<*> z
magic4 f x y z a0 = f Lola.<$> x Lola.<*> y Lola.<*> z Lola.<*> a0
magic5 f x y z a0 a1 = f Lola.<$> x Lola.<*> y Lola.<*> z Lola.<*> a0 Lola.<*> a1
magic6 f x y z a0 a1 a2= f Lola.<$> x Lola.<*> y Lola.<*> z Lola.<*> a0 Lola.<*> a1 Lola.<*> a2
magic7 f x y z a0 a1 a2 a3 = f Lola.<$> x Lola.<*> y Lola.<*> z Lola.<*> a0 Lola.<*> a1 Lola.<*> a2 Lola.<*> a3

emap :: Typeable a => (x->Expr a) -> [x] -> Expr [a]
emap f ls = let
  exprs = map f ls
  in foldr (\ea els -> (:) <$> ea <*> els) (Leaf []) exprs
