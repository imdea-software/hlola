{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Theories.Booleans3 where

import Syntax.HLPrelude
import Data.Aeson
import GHC.Generics
import qualified Prelude as P

data Bool3 = True3 | DK3 | False3 deriving (Show,Generic,Read,FromJSON,ToJSON)

fromBool :: Bool -> Bool3
fromBool True = True3
fromBool False = False3

infixr 2 ||
(||) :: Bool3 -> Bool3 -> Bool3
True3  || _      = True3
_      || True3  = True3
False3 || False3 = False3
_      || _      = DK3

infixr 3 &&
(&&) :: Bool3 -> Bool3 -> Bool3
False3 && _      = False3
_      && False3 = False3
True3  && True3  = True3
_      && _      = DK3

not :: Bool3 -> Bool3
not True3  = False3
not False3 = True3
not DK3    = DK3
