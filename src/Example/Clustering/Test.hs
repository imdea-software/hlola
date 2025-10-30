{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.Clustering.Test where
import Data.Aeson
import DecDyn
import GHC.Generics
import Lola
import Syntax.Booleans
import Syntax.HLPrelude
import Syntax.Num
import Syntax.Ord
import Lib.DynPar
import Lib.Utils
import qualified Data.Set as Set
import qualified Prelude as P
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Dynamic
import qualified Data.ByteString.Lazy as B
import TraceRetrieving
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace

import Data.Text.Lazy             as TL

import Data.Text.Lazy.Encoding    as TL
import Data.Text.Lazy.IO          as TL

spec :: Specification
spec = [out testystr, out lastblock]

block :: Stream Int
block = Input "block"

lastblock :: Stream Int
lastblock = "lastblock" =: block :@ (-1,-1)

testystr :: Stream Int
testystr = "testystr" =: testystr :@ (-3,0)*2 + Now block + Now lastblock * 0

myIniter = undefined
-- myIniter :: Initer () Int
-- myIniter _ _ futevs _ = let
--   prevevs = [(fromMaybe (error "No json").decode) (TL.encodeUtf8 $ TL.pack "{\"block\":24}"), (fromMaybe (error "No json").decode) (TL.encodeUtf8 $ TL.pack "{\"block\":34}"), (fromMaybe (error "No json").decode) (TL.encodeUtf8 $ TL.pack "{\"block\":2100}")]
--   in initWithPast testystr prevevs futevs

mainpast :: [Map.Map Ident Dynamic]
mainpast = fst$myIniter undefined 0 [] ()
