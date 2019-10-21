module Syntax.Booleans3 where

import Syntax.HLPrelude
import qualified Prelude as P
import qualified Theories.Booleans3 as B3

import Lola

infixr 2 ||
(||) :: Expr B3.Bool3 -> Expr B3.Bool3 -> Expr B3.Bool3
a || b = (B3.||) <$> a <*> b

infixr 3 &&
(&&) :: Expr B3.Bool3 -> Expr B3.Bool3 -> Expr B3.Bool3
a && b = (B3.&&) <$> a <*> b

not :: Expr B3.Bool3 -> Expr B3.Bool3
not a = B3.not <$> a
