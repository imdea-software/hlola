{-# LANGUAGE DeriveGeneric #-}
{-#LANGUAGE ScopedTypeVariables#-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE FlexibleInstances#-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric, DeriveAnyClass #-}
module Interpreter.AST where
import GHC.Generics
import Data.Aeson
import Data.Typeable (Proxy)
import Type.Reflection
import Control.Unification
import Control.Unification.IntVar
import Control.Unification.Types
import Control.Monad.Trans.Except
import Data.Functor.Identity
import Data.Fix (Fix(..))
import Data.Functor.Classes
import qualified Data.Map.Lazy as Map
import Data.Constraint (Dict)
import Data.Default

type SymbolStr = String
type Type = Fix TypeF
data GBody a = 
  Symbol a
  | Application (GBody a) (GBody a)
  deriving (Generic,FromJSON,Show,ToJSON,Functor)

type Body = GBody SymbolStr

data StrDec = StrDec {
  isoutput :: Bool,
  name :: String,
  typ :: Type,
  params :: [(SymbolStr, Type)],
  body :: Maybe Body
  } deriving (Generic,FromJSON,Show,ToJSON)

type Spec = [StrDec]

data TypeF a = 
  TBase String
  | TApp a a
  deriving (Functor, Foldable, Traversable, Show, Generic1, Unifiable, Generic, FromJSON1, ToJSON1)

instance Show1 TypeF where
    liftShowsPrec _ _ _ (TBase x) _ = "(TBase " ++ x ++ ")"
    -- Binary ops are wrong
    liftShowsPrec sp _ d (TApp x y) a =
                showsBinaryWith sp sp "TApp" d x y a

type MyFailure = String
type OrFailure a = Either MyFailure a
-- Definition of ExceptT:
-- newtype ExceptT e m a = ExceptT (m (Either e a))
type TMonadic a = ExceptT (UFailure TypeF IntVar) (IntBindingT TypeF Identity) a
type PolyType = UTerm TypeF IntVar
type ThFun = TMonadic KnownVal
type ThEntry = TMonadic (PolyType, ThFun)
type TThEntry = TypeDB -> ThEntry
type Theory = Map.Map String TThEntry

class TypeEntry a where
  toKTD :: a -> OrFailure KTD
  toFun :: a -> OrFailure (KTD -> DTypeEntry)
instance TypeEntry KTD where
  toKTD x = return x
  toFun (KTD (_ :: Proxy ty) _) = let
    errormsg = "Not a type constructor: " ++ show (typeRep :: TypeRep ty)
    in Left errormsg
instance TypeEntry a => TypeEntry (KTD -> a) where
  toKTD _ = let
    errormsg = "Not a base type."
    in Left errormsg
  toFun f = Right (\ktd -> DTE (f ktd))

data DTypeEntry where
  DTE :: forall (a :: * ) . TypeEntry a => a -> DTypeEntry

type TypeDB = Map.Map String DTypeEntry

data KnownType where
  KnownType :: forall (a :: * ) . Typeable a => Proxy a -> KnownType

data KnownVal where
  KnownVal :: forall (a :: * ) . Typeable a => a -> KnownVal

data KTD where
  KTD :: forall (a :: * ) . Typeable a => Proxy a -> Dicts a -> KTD

type MD ctx a = Maybe (Dict (ctx a))

data Dicts a = Dicts {
  meqdict :: MD Eq a
  ,mshowdict :: MD Show a
  ,morddict :: MD Ord a
  ,mfromjsdict :: MD FromJSON a
  ,mreaddict :: MD Read a
  ,mtojsdict :: MD ToJSON a
  ,mdfltdict :: MD Default a
  }

orfeqdict :: Typeable a => Dicts a -> OrFailure (Dict (Eq a))
orfeqdict dict = dictorfail "Eq" meqdict dict
orfshowdict dict = dictorfail "Show" mshowdict dict
orforddict dict = dictorfail "Ord" morddict dict
orffromjsdict dict = dictorfail "FromJS" mfromjsdict dict
orfreaddict dict = dictorfail "Read" mreaddict dict
orftojsdict dict = dictorfail "ToJS" mtojsdict dict
orfdfltdict dict = dictorfail "Dflt" mdfltdict dict

dictorfail :: Typeable a => String -> (Dicts a -> MD ctx a) -> Dicts a -> OrFailure (Dict (ctx a))
dictorfail ctxname getter (dicts :: Dicts a) = let
  errmsg = "The type " ++ show (typeRep @a) ++ " is not an instance of " ++ ctxname
  in mb2orf errmsg $ getter dicts

mb2orf :: String -> Maybe a -> OrFailure a
mb2orf err Nothing = Left err
mb2orf _ (Just x) = Right x

orf2mon :: OrFailure a -> TMonadic a
orf2mon (Right x) = return x
orf2mon (Left err) = error err

runTMonadic :: TMonadic a -> Either (UFailure TypeF IntVar) a
runTMonadic = runIdentity . evalIntBindingT . runExceptT
