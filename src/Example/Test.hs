{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.Test where

import Lola
import Syntax.HLPrelude
import DecDyn
import Syntax.Booleans
import Lib.Utils
import qualified Prelude as P

spec :: Specification
spec = [out dadiv]

dadiv :: Stream Int
dadiv = "dadiv" =: if Leaf True then 0 else div <$> 4 <*> 0
