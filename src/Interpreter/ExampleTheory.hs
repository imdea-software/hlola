{-#LANGUAGE ScopedTypeVariables#-}
{-#LANGUAGE TypeApplications #-}
module Interpreter.ExampleTheory where
import Interpreter.Theory
import Theories.TCPState
import Data.Default
import Interpreter.AST
import Data.Proxy
import Syntax.Booleans (lfand,lfor)
import Syntax.Booleans (itelfun)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.Unification
import Data.Constraint (Dict(..), (\\) )
import Lib.Utils (instantN, plusone, instantPlusN, pluser, instantiator5)
import Lola (Stream, Expr(Now,Leaf,(:@),App), getMayber, toLFunction, LFunction)
import qualified Lib.DynPar as DP (over)
import Data.Maybe
import Data.Typeable
import Data.Fix
import Debug.Trace
import Data.Aeson

setEntry :: KTD -> KTD
setEntry (KTD (_ :: Proxy a) (Dicts meqa mshowa morda mfja mreada mtja mdflta)) = let
  mydicts = Dicts {
    meqdict = (Dict \\) <$> meqa
    ,mshowdict = (Dict \\) <$> mshowa
    ,morddict = (Dict \\) <$> morda
    ,mfromjsdict = ((Dict \\) \\) <$> morda <*> mfja
    ,mreaddict = ((Dict \\) \\) <$> morda <*> mreada
    ,mtojsdict = (Dict \\) <$> mtja
    ,mdfltdict = Just Dict
    }
  in (KTD (Proxy @(Set.Set a)) mydicts)

streamEntry :: KTD -> KTD
streamEntry (KTD (_ :: Proxy a) dicts) = let
  mydicts = Dicts Nothing (Just Dict) Nothing Nothing Nothing Nothing Nothing
  in (KTD (Proxy @(Stream a)) mydicts)

listEntry :: KTD -> KTD
listEntry (KTD (_ :: Proxy a) (Dicts meqa mshowa morda mfja mreada mtja mdflta)) = let
  mydicts = Dicts {
    meqdict = (Dict \\) <$> meqa
    ,mshowdict = (Dict \\) <$> mshowa
    ,morddict = (Dict \\) <$> morda
    ,mfromjsdict = (Dict \\) <$> mfja
    ,mreaddict = (Dict \\) <$> mreada
    ,mtojsdict = (Dict \\) <$> mtja
    ,mdfltdict = Just Dict
    }
  in (KTD (Proxy @[a]) mydicts)

mapEntry :: KTD -> KTD -> KTD
mapEntry (KTD (_ :: Proxy k) (Dicts meqk mshowk mordk mfjk mreadk mtjk mdfltk)) (KTD (_ :: Proxy v) (Dicts meqv mshowv mordv mfjv mreadv mtjv mdfltv)) = let
  mydicts = Dicts {
    meqdict = ((Dict \\) \\) <$> meqk <*> meqv
    ,mshowdict = ((Dict \\) \\) <$> mshowk <*> mshowv
    ,morddict = ((Dict \\) \\) <$> mordk <*> mordv
    ,mfromjsdict = Nothing -- TODO
    ,mreaddict = (((Dict \\) \\) \\) <$> mordk <*> mreadk <*> mreadv
    ,mtojsdict = Nothing -- TODO
    ,mdfltdict = Just Dict
    }
  in (KTD (Proxy @(Map.Map k v)) mydicts)

pairEntry :: KTD -> KTD -> KTD
pairEntry (KTD (_ :: Proxy a) (Dicts meqa mshowa morda mfja mreada mtja mdflta)) (KTD (_ :: Proxy b) (Dicts meqb mshowb mordb mfjb mreadb mtjb mdfltb)) = let
  mydicts = Dicts {
    meqdict = ((Dict \\) \\) <$> meqa <*> meqb
    ,mshowdict = ((Dict \\) \\) <$> mshowa <*> mshowb
    ,morddict = ((Dict \\) \\) <$> morda <*> mordb
    ,mfromjsdict = ((Dict \\) \\) <$> mfja <*> mfjb
    ,mreaddict = ((Dict \\) \\) <$> mreada <*> mreadb
    ,mtojsdict = ((Dict \\) \\) <$> mtja <*> mtjb
    ,mdfltdict = ((Dict \\) \\) <$> mdflta <*> mdfltb
    }
  in KTD (Proxy @(a,b)) mydicts

iteinfo :: TThEntry
iteinfo tdb = do
  a <- freshvar
  let lfun a b = ground (Proxy @LFunction) `polyapp` a `polyapp` b
  let ty = ground (Proxy @Bool) `lfun` (a `lfun` (a `lfun` a))
  return (ty, monadicfun a)
  where
  monadicfun :: PolyType -> TMonadic KnownVal
  monadicfun var = do
    tx <- applyBindings var
    x <- orf2mon$poly2type tx
    KnownType (_ :: Proxy a) <- orf2mon$getKT tdb x
    let dafun = itelfun @a
    return $ KnownVal dafun

eqinfo :: TThEntry
eqinfo tdb = do
  a <- freshvar
  let ty = a ~> a ~> ground (Proxy @Bool)
  return (ty, monadicfun a)
  where
  monadicfun :: PolyType -> TMonadic KnownVal
  monadicfun var = do
    tx <- applyBindings var
    x <- orf2mon$poly2type tx
    (KTD (_ :: Proxy a) dicts) <- orf2mon $ getKTD tdb x
    eqdict <- orf2mon $ orfeqdict dicts
    let dafun = (==) @a \\ eqdict
    return $ KnownVal dafun

neqinfo :: TThEntry
neqinfo tdb = do
  a <- freshvar
  let ty = a ~> a ~> ground (Proxy @Bool)
  return (ty, monadicfun a)
  where
  monadicfun :: PolyType -> TMonadic KnownVal
  monadicfun var = do
    tx <- applyBindings var
    x <- orf2mon$poly2type tx
    (KTD (_ :: Proxy a) dicts) <- orf2mon $ getKTD tdb x
    eqdict <- orf2mon $ orfeqdict dicts
    let dafun = (/=) @a \\ eqdict
    return $ KnownVal dafun

ltinfo :: TThEntry
ltinfo tdb = do
  a <- freshvar
  let ty = a ~> a ~> ground (Proxy @Bool)
  return (ty, monadicfun a)
  where
  monadicfun :: PolyType -> TMonadic KnownVal
  monadicfun var = do
    tx <- applyBindings var
    x <- orf2mon$poly2type tx
    (KTD (_ :: Proxy a) dicts) <- orf2mon $ getKTD tdb x
    orddict <- orf2mon $ orforddict dicts
    let dafun = (<) @a \\ orddict
    return $ KnownVal dafun

setfromlistinfo :: TThEntry
setfromlistinfo tdb = do
  a <- freshvar
  let ty = ground (Proxy @[]) `polyapp` a ~> ground (Proxy @Set.Set) `polyapp` a
  return (ty, monadicfun a)
  where
  monadicfun :: PolyType -> TMonadic KnownVal
  monadicfun var = do
    tx <- applyBindings var
    x <- orf2mon$poly2type tx
    (KTD (_ :: Proxy a) dicts) <- orf2mon $ getKTD tdb x
    orddict <- orf2mon $ orforddict dicts
    let dafun = Set.fromList @a \\ orddict
    return $ KnownVal dafun

setdeleteinfo :: TThEntry
setdeleteinfo tdb = do
  a <- freshvar
  let seta = ground (Proxy @Set.Set) `polyapp` a
  let ty = a ~> seta ~> seta
  return (ty, monadicfun a)
  where
  monadicfun :: PolyType -> TMonadic KnownVal
  monadicfun var = do
    tx <- applyBindings var
    x <- orf2mon$poly2type tx
    (KTD (_ :: Proxy a) dicts) <- orf2mon $ getKTD tdb x
    orddict <- orf2mon $ orforddict dicts
    let dafun = Set.delete @a \\ orddict
    return $ KnownVal dafun

setinsertinfo :: TThEntry
setinsertinfo tdb = do
  a <- freshvar
  let seta = ground (Proxy @Set.Set) `polyapp` a
  let ty = a ~> seta ~> seta
  return (ty, monadicfun a)
  where
  monadicfun :: PolyType -> TMonadic KnownVal
  monadicfun var = do
    tx <- applyBindings var
    x <- orf2mon$poly2type tx
    (KTD (_ :: Proxy a) dicts) <- orf2mon $ getKTD tdb x
    orddict <- orf2mon $ orforddict dicts
    let dafun = Set.insert @a \\ orddict
    return $ KnownVal dafun

setsizeinfo :: TThEntry
setsizeinfo tdb = do
  a <- freshvar
  let seta = ground (Proxy @Set.Set) `polyapp` a
  let ty = seta ~> ground (Proxy @Int)
  return (ty, monadicfun a)
  where
  monadicfun :: PolyType -> TMonadic KnownVal
  monadicfun var = do
    tx <- applyBindings var
    x <- orf2mon$poly2type tx
    KnownType (_ :: Proxy a) <- orf2mon$ getKT tdb x
    let dafun = Set.size @a
    return $ KnownVal dafun

emptylistinfo :: TThEntry
emptylistinfo tdb = do
  a <- freshvar
  let ty = ground (Proxy @[]) `polyapp` a
  return (ty, monadicfun a)
  where
  monadicfun :: PolyType -> TMonadic KnownVal
  monadicfun var = do
    tx <- applyBindings var
    x <- orf2mon$poly2type tx
    KnownType (_ :: Proxy a) <- orf2mon$ getKT tdb x
    let dafun = [] @a
    return $ KnownVal dafun

listconsinfo :: TThEntry
listconsinfo tdb = do
  a <- freshvar
  let tylista = ground (Proxy @[]) `polyapp` a
  let ty = a ~> tylista ~> tylista
  return (ty, monadicfun a)
  where
  monadicfun :: PolyType -> TMonadic KnownVal
  monadicfun var = do
    tx <- applyBindings var
    x <- orf2mon$poly2type tx
    KnownType (_ :: Proxy a) <- orf2mon$ getKT tdb x
    let dafun = (:) @a
    return $ KnownVal dafun

listmaxinfo :: TThEntry
listmaxinfo tdb = do
  a <- freshvar
  let lista = ground (Proxy @[]) `polyapp` a
  let ty = lista ~> a
  return (ty, monadicfun a)
  where
  monadicfun :: PolyType -> TMonadic KnownVal
  monadicfun var = do
    tx <- applyBindings var
    x <- orf2mon$poly2type tx
    (KTD (_ :: Proxy a) dicts) <- orf2mon $ getKTD tdb x
    orddict <- orf2mon $ orforddict dicts
    let dafun = (maximum :: [a] -> a) \\ orddict
    return $ KnownVal dafun

lengthinfo :: TThEntry
lengthinfo tdb = do
  a <- freshvar
  let lista = ground (Proxy @[]) `polyapp` a
  let ty = lista ~> ground (Proxy @Int)
  return (ty, monadicfun a)
  where
  monadicfun :: PolyType -> TMonadic KnownVal
  monadicfun var = do
    tx <- applyBindings var
    x <- orf2mon$poly2type tx
    (KTD (_ :: Proxy a) dicts) <- orf2mon $ getKTD tdb x
    orddict <- orf2mon $ orforddict dicts
    let dafun = length :: [a] -> Int
    return $ KnownVal dafun

mapinfo :: TThEntry
mapinfo tdb = do
  a <- freshvar
  b <- freshvar
  let lista = ground (Proxy @[]) `polyapp` a
  let listb = ground (Proxy @[]) `polyapp` b
  let ty = (a~>b) ~> lista ~> listb
  return (ty, monadicfun a b)
  where
  monadicfun :: PolyType -> PolyType -> TMonadic KnownVal
  monadicfun vara varb = do
    tx <- applyBindings vara
    x <- orf2mon$poly2type tx
    ty <- applyBindings varb
    y <- orf2mon$poly2type ty
    KnownType (_ :: Proxy a) <- orf2mon$getKT tdb x
    KnownType (_ :: Proxy b) <- orf2mon$getKT tdb y
    let dafun = map @a @b
    return $ KnownVal dafun

mkpairinfo :: TThEntry
mkpairinfo tdb = do
  a <- freshvar
  b <- freshvar
  let typairab = ground (Proxy @(,)) `polyapp` a `polyapp` b
  let ty = a ~> b ~> typairab
  return (ty, monadicfun a b)
  where
  monadicfun :: PolyType -> PolyType -> TMonadic KnownVal
  monadicfun vara varb = do
    ta <- applyBindings vara
    tb <- applyBindings varb
    x <- orf2mon$poly2type ta
    y <- orf2mon$poly2type tb
    KnownType (_ :: Proxy a) <- orf2mon$getKT tdb x
    KnownType (_ :: Proxy b) <- orf2mon$getKT tdb y
    let dafun = (,) @a @b
    return $ KnownVal dafun

zipinfo :: TThEntry
zipinfo tdb = do
  a <- freshvar
  b <- freshvar
  let tylista = ground (Proxy @[]) `polyapp` a
  let tylistb = ground (Proxy @[]) `polyapp` b
  let tylspairab = ground (Proxy @[]) `polyapp` (ground (Proxy @(,)) `polyapp` a `polyapp` b)
  let ty = tylista ~> tylistb ~> tylspairab
  return (ty, monadicfun a b)
  where
  monadicfun :: PolyType -> PolyType -> TMonadic KnownVal
  monadicfun vara varb = do
    ta <- applyBindings vara
    tb <- applyBindings varb
    x <- orf2mon$poly2type ta
    y <- orf2mon$poly2type tb
    KnownType (_ :: Proxy a) <- orf2mon$getKT tdb x
    KnownType (_ :: Proxy b) <- orf2mon$getKT tdb y
    let dafun = zip @a @b
    return $ KnownVal dafun

zipWithinfo :: TThEntry
zipWithinfo tdb = do
  a <- freshvar
  b <- freshvar
  c <- freshvar
  let tylista = ground (Proxy @[]) `polyapp` a
  let tylistb = ground (Proxy @[]) `polyapp` b
  let tylistc = ground (Proxy @[]) `polyapp` c
  let ty = (a ~> b ~> c) ~> tylista ~> tylistb ~> tylistc
  return (ty, monadicfun a b c)
  where
  monadicfun :: PolyType -> PolyType -> PolyType -> TMonadic KnownVal
  monadicfun vara varb varc = do
    ta <- applyBindings vara
    tb <- applyBindings varb
    tc <- applyBindings varc
    x <- orf2mon$poly2type ta
    y <- orf2mon$poly2type tb
    z <- orf2mon$poly2type tc
    KnownType (_ :: Proxy a) <- orf2mon$getKT tdb x
    KnownType (_ :: Proxy b) <- orf2mon$getKT tdb y
    KnownType (_ :: Proxy c) <- orf2mon$getKT tdb z
    let dafun = zipWith @a @b @c
    return $ KnownVal dafun

uncurryinfo :: TThEntry
uncurryinfo tdb = do
  a <- freshvar
  b <- freshvar
  c <- freshvar
  let typairab = ground (Proxy @(,)) `polyapp` a `polyapp` b
  let ty = (a~>b~>c) ~> typairab ~> c
  return (ty, monadicfun a b c)
  where
  monadicfun :: PolyType -> PolyType -> PolyType -> TMonadic KnownVal
  monadicfun vara varb varc = do
    ta <- applyBindings vara
    tb <- applyBindings varb
    tc <- applyBindings varc
    x <- orf2mon$poly2type ta
    y <- orf2mon$poly2type tb
    z <- orf2mon$poly2type tc
    KnownType (_ :: Proxy a) <- orf2mon$getKT tdb x
    KnownType (_ :: Proxy b) <- orf2mon$getKT tdb y
    KnownType (_ :: Proxy c) <- orf2mon$getKT tdb z
    let dafun = uncurry @a @b @c
    return $ KnownVal dafun

elemsinfo :: TThEntry
elemsinfo tdb = do
  a <- freshvar
  b <- freshvar
  let tymapba = ground (Proxy @Map.Map) `polyapp` b `polyapp` a
  let tylista = ground (Proxy @[]) `polyapp` a
  let ty = tymapba ~> tylista
  return (ty, monadicfun a b)
  where
  monadicfun :: PolyType -> PolyType -> TMonadic KnownVal
  monadicfun vara varb = do
    ta <- applyBindings vara
    tb <- applyBindings varb
    x <- orf2mon$poly2type ta
    y <- orf2mon$poly2type tb
    KnownType (_ :: Proxy a) <- orf2mon$getKT tdb x
    KnownType (_ :: Proxy b) <- orf2mon$getKT tdb y
    let dafun = Map.elems @b @a
    return $ KnownVal dafun

tolistinfo :: TThEntry
tolistinfo tdb = do
  a <- freshvar
  b <- freshvar
  let typairab = ground (Proxy @(,)) `polyapp` a `polyapp` b
  let tymapab = ground (Proxy @Map.Map) `polyapp` a `polyapp` b
  let tylistab = ground (Proxy @[]) `polyapp` typairab
  let ty = tymapab ~> tylistab
  return (ty, monadicfun a b)
  where
  monadicfun :: PolyType -> PolyType -> TMonadic KnownVal
  monadicfun vara varb = do
    ta <- applyBindings vara
    tb <- applyBindings varb
    x <- orf2mon$poly2type ta
    y <- orf2mon$poly2type tb
    KnownType (_ :: Proxy a) <- orf2mon$getKT tdb x
    KnownType (_ :: Proxy b) <- orf2mon$getKT tdb y
    let dafun = Map.toList @a @b
    return $ KnownVal dafun

-- =-=-=-=-=-=-=-=- Expr constructors
lfunEntry :: KTD -> KTD -> KTD
lfunEntry (KTD (_ :: Proxy a) _) (KTD (_ :: Proxy b) _) =
  (KTD (Proxy @(LFunction a b)) (Dicts Nothing Nothing Nothing Nothing Nothing Nothing Nothing))

exprpt = polyapp (ground (Proxy @Expr))
streampt = polyapp (ground (Proxy @Stream))

nowinfo :: TThEntry
nowinfo tdb = do
  a <- freshvar
  let ty = exprpt (streampt a) ~> exprpt a
  return (ty, monadicfun a)
  where
  monadicfun :: PolyType -> TMonadic KnownVal
  monadicfun var = do
    tx <- applyBindings var
    x <- orf2mon$poly2type tx
    KnownType (_ :: Proxy a) <- orf2mon$ getKT tdb x
    let dafun = (\es -> Now (solveExpr es :: Stream a))
    return $ KnownVal dafun

leafinfo :: TThEntry
leafinfo tdb = do
  a <- freshvar
  let ty = a ~> exprpt a
  return (ty, monadicfun a)
  where
  monadicfun :: PolyType -> TMonadic KnownVal
  monadicfun var = do
    tx <- applyBindings var
    x <- orf2mon$poly2type tx
    KnownType (_ :: Proxy a) <- orf2mon$ getKT tdb x
    let dafun = Leaf @a
    return $ KnownVal dafun

solveExpr :: Lola.Expr ty -> ty
solveExpr (Leaf x) = x
solveExpr (Lola.App elf earg) = let
  lf = solveExpr elf
  arg = solveExpr earg
  f = (getMayber . toLFunction) lf
  in fromJust $ f (Just arg)
solveExpr (Now x) = error "Trying to solve a Now in static time"
solveExpr (x :@ (y,z)) = error "Trying to solve an At in static time"

atinfo :: TThEntry
atinfo tdb = do
  a <- freshvar
  let ty = exprpt (streampt a) ~> exprpt (ground (Proxy @Int)) ~> exprpt a ~> exprpt a
  return (ty, monadicfun a)
  where
  monadicfun :: PolyType -> TMonadic KnownVal
  monadicfun var = do
    tx <- applyBindings var
    x <- orf2mon$poly2type tx
    KnownType (_ :: Proxy a) <- orf2mon$ getKT tdb x
    let dafun = eat @a
    return $ KnownVal dafun
    where
    eat :: Expr (Stream a) -> Expr Int -> Expr a -> Expr a
    eat es eix df = let
      s = solveExpr es
      ix = solveExpr eix
      in s :@(ix,df)

appinfo :: TThEntry
appinfo tdb = do
  a <- freshvar
  b <- freshvar
  f <- freshvar
  let ty = exprpt (f `polyapp` a `polyapp` b) ~> exprpt a ~> exprpt b
  return (ty, monadicfun f a b)
  where
  monadicfun :: PolyType -> PolyType -> PolyType -> TMonadic KnownVal
  monadicfun varf vara varb = do
    tf <- applyBindings varf
    ta <- applyBindings vara
    tb <- applyBindings varb
    ff <- orf2mon$poly2type tf
    x <- orf2mon$poly2type ta
    y <- orf2mon$poly2type tb
    KnownType (_ :: Proxy a) <- orf2mon$getKT tdb x
    KnownType (_ :: Proxy b) <- orf2mon$getKT tdb y
    let dafun = appfun @a @b
    let dalfun = applfun @a @b
    let kvfun = KnownVal dafun
    let kvlfun = KnownVal dalfun
    return $ (if islfun ff then kvlfun else kvfun)
    where
    islfun (Fix (TBase "LFunction")) =  True
    islfun ty = False
    appfun :: (Typeable x, Typeable y) => Expr (x->y) -> Expr x -> Expr y
    appfun = Lola.App
    applfun :: (Typeable x, Typeable y) => Expr (LFunction x y) -> Expr x -> Expr y
    applfun = Lola.App

overinfo :: TThEntry
overinfo tdb = do
  x <- freshvar
  a <- freshvar
  let pstrmty = x ~> streampt a
  let argsetty = streampt (ground (Proxy @Set.Set) `polyapp` x)
  let retty = exprpt (ground (Proxy @Map.Map) `polyapp` x `polyapp` a)
  let ty = exprpt pstrmty ~> exprpt argsetty ~> retty
  return (ty, monadicfun x a)
  where
  monadicfun :: PolyType -> PolyType -> TMonadic KnownVal
  monadicfun varx vara = do
    tx <- applyBindings varx
    ta <- applyBindings vara
    x <- orf2mon$poly2type tx
    a <- orf2mon$poly2type ta
    (KTD (_ :: Proxy rx) dictsx) <- orf2mon $ getKTD tdb x
    ordxdict <- orf2mon $ orforddict dictsx
    showxdict <- orf2mon $ orfshowdict dictsx
    tojsxdict <- orf2mon $ orftojsdict dictsx
    dfltxdict <- orf2mon $ orfdfltdict dictsx
    (KTD (_ :: Proxy ra) dictsa) <- orf2mon $ getKTD tdb a
    showadict <- orf2mon $ orfshowdict dictsa
    tojsadict <- orf2mon $ orftojsdict dictsa
    let dafun = overfun @ra @rx \\ ordxdict \\ showxdict \\ showadict \\ tojsadict \\ tojsxdict \\ dfltxdict
    return $ KnownVal dafun
    where
    overfun :: (Typeable a, Typeable x, Ord x, Show x, Show a, ToJSON a, ToJSON x, Default x) => Expr (x->Stream a) -> Expr (Stream (Set.Set x)) -> Expr (Map.Map x a)
    overfun eps eargs = let
      ps = solveExpr eps
      args = solveExpr eargs
      in DP.over ps args

-- -=-=-=-=-=-=-=-=-=-= Example theory:

typeConstructors :: TheoryBuilder
typeConstructors =
  addTypeEntry "Declaration" (DTE streamEntry) >>
  typeSynonym "Stream" (Proxy @Stream) >>
  addTypeEntry "Set" (DTE setEntry) >>
  addTypeEntry "[]" (DTE listEntry) >>
  addTypeEntry "(,)" (DTE pairEntry) >>
  addTypeEntry "Map" (DTE mapEntry)

inttheory :: TheoryBuilder
inttheory = do
  addType (Proxy @Int)
  addSymbol "successor" $ totheory ((+1)::Int->Int)
  addSymbol "summer" $ totheory ((\(x,y) -> x+y)::(Int,Int)->Int)
  addSymbol "min" $ totheory (min @Int)
  addOperator "+" 2 $ totheory ((+) @Int)
  addOperator "-" 2 $ totheory ((-) @Int)
  addOperator "*" 3 $ totheory ((*) @Int)
  addOperator "/" 3 $ totheory (div @Int)
  addSymbol "MAXINT" $ totheory (1000000::Int)

instance Default Char where
  def = 'c'

stringtheory :: TheoryBuilder
stringtheory = 
  addType (Proxy @Char) >>
  typeSynonym "String" (Proxy @String)

instance Default Bool where
  def = True

booltheory :: TheoryBuilder
booltheory = do
  addType (Proxy @Bool)
  addSymbol "not" $ totheory not
  addSymbol "or" $ totheory lfor
  addSymbol "and" $ totheory lfand
  addOperator "&&" 3 $ totheory lfand
  addOperator "||" 3 $ totheory lfor

polyfuns :: TheoryBuilder
polyfuns =
  addSymbol "ite" iteinfo >>
  addOperator "==" 2 eqinfo >>
  addOperator "/=" 2 neqinfo >>
  addOperator "<" 2 ltinfo >>
  addSymbol "setFromList" setfromlistinfo >>
  addSymbol "insert" setinsertinfo >>
  addSymbol "delete" setdeleteinfo >>
  addSymbol "size" setsizeinfo >>
  addSymbol "emptylist" emptylistinfo >>
  addSymbol "listcons" listconsinfo >>
  addSymbol "length" lengthinfo >>
  addSymbol "listmax" listmaxinfo >>
  addSymbol "length" lengthinfo >>
  addSymbol "map" mapinfo >>
  addSymbol "mkpair" mkpairinfo >>
  addSymbol "zip" zipinfo >>
  addSymbol "zipWith" zipWithinfo >>
  addSymbol "uncurry" uncurryinfo >>
  addSymbol "elems" elemsinfo >>
  addSymbol "toList" tolistinfo -- Map elems

tcptheory :: TheoryBuilder
tcptheory = do
  addSymbol "getNextState" $ totheory getNextState
  addType (Proxy @TCPState)
  addType (Proxy @PacketType)
  addSymbol "Error" $ totheory TCPError

externalstreams :: TheoryBuilder
externalstreams = do
  addSymbol "instantN" $ totheory instantN
  addSymbol "plusone" $ totheory plusone
  addSymbol "instantiator5" $ totheory instantiator5
  addSymbol "pluser" $ totheory pluser
  addSymbol "instantPlusN" $ totheory instantPlusN

exprconstructors :: TheoryBuilder
exprconstructors = 
  addTypeEntry "LFunction" (DTE lfunEntry) >>
  addSymbol "now" nowinfo >>
  addSymbol "at" atinfo >>
  addSymbol "over" overinfo >>
  addSymbol "app" appinfo >>
  addSymbol "leaf" leafinfo

exampletheory :: TheoryBuilder
exampletheory = 
  typeConstructors >>
  polyfuns >>
  inttheory >>
  booltheory >>
  stringtheory >>
  tcptheory >>
  externalstreams >>
  exprconstructors
