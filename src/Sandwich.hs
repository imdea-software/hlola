{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Sandwich where
import Data.Aeson
import DecDyn
import GHC.Generics
import Lola
import Syntax.Booleans
import Syntax.HLPrelude hiding (error)
import Syntax.Num
import Syntax.Ord
import Lib.DynPar
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Prelude as P
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.Dynamic
import DecDyn
import System.IO.Unsafe (unsafePerformIO)
import TraceRetrieving
import System.Environment (getArgs, getExecutablePath)
import Engine.Engine

type Transactions = Map.Map String String

callDistance = 40 :: Int

level :: Stream Int
level = Input "level"

hash :: Stream String
hash = Input "hash"

senderaddr :: Stream String
senderaddr = Input "senderaddress"

targetaddr :: Stream String
targetaddr = Input "targetaddress"

paramentry :: Stream String
paramentry = Input "paramentry"

atokenamt :: Stream String
atokenamt = Input "atokenamt"

btokenamt :: Stream String
btokenamt = Input "btokenamt"

scriptPath :: String
scriptPath = let
  binpath = unsafePerformIO getExecutablePath
  localpath = reverse $ dropWhile (/= '/') $ reverse binpath
  in localpath ++ "Tezos/processBlock.sh"

-- Address for 3route v4  
routeaddress = "KT1V5XKmeypanMS9pR65REpqmVejWBZURuuT"

spec :: Specification

spec = [out targetaddr, out paramentry, out isSandwichAttackPrint, out whoSandwichAttack, out atokenamt, out btokenamt, out setSuspicious, out level, out called3routePrint, out count3route, out countAttacks]

-- Transaction counter
counterTr :: Stream Int
counterTr = "counterTr" =:
  counterTr :@ (-1, 0) + 1

-- Counter for calls to 3route
count3route :: Stream Int
count3route = "count3route" =:
  count3route :@ (-1,0) + if Now called3route then 1 else 0

-- Counter for attacks
countAttacks :: Stream Int
countAttacks = "countAttacks" =:
  countAttacks :@ (-1,0) + if Now isSandwichAttack then 1 else 0

-- Boolean output for Sandwich Attack
isSandwichAttack :: Stream Bool
isSandwichAttack = "isSandwichAttack" =:
  Now called3route && not (Map.null <$> Now willSwapTwice)

-- String output for Sandwich Attack (for easy grep)
isSandwichAttackPrint :: Stream String
isSandwichAttackPrint = "isSandwichAttackPrint" =:
  if Now isSandwichAttack then
    Leaf "AttackYes"
  else
    Leaf "AttackNo"

-- Malicious address origin of the sandwich attack
whoSandwichAttack :: Stream String
whoSandwichAttack = "whoSandwichAttack" =:
  if Now called3route && Now isSandwichAttack then
    myElem <$> Now willSwapTwice <*> Now attacker
  else
    Leaf "No attack"
  where
    myElem l a = "Sandwich attack by " ++ a

-- Potential attacker address      
attacker :: Stream String 
attacker = "attacker" =:
  if Now called3route && not (Map.null <$> Now willSwapTwice) then
    myElem <$> Now willSwapTwice
  else
    Leaf ""
  where
    myElem l = fst (head (Map.toList l))

-- Get Map with transactions with do_swap
getSwapMap :: Stream Transactions
getSwapMap = "getSwapMap" =:
  if (Now paramentry === Leaf "do_swap") then
    updateMap <$> Now targetaddr <*> Now btokenamt <*> prevMap
  else
    prevMap
  where
    updateMap add amt prevM = Map.insert add amt prevM
    prevMap = getSwapMap :@ (-1, Leaf Map.empty)
      
willSwapTwice :: Stream Transactions
willSwapTwice = "willSwapTwice" =:
  if Now called3route then
    rs <$> (listAddressesPast <$> Now level <*> Now counterTr) <*> (listAddressesFuture <$> (paramentry :@@ (Leaf callDistance)) <*> (targetaddr :@@ (Leaf callDistance)) <*> (atokenamt :@@ (Leaf callDistance)))
  else
    Leaf Map.empty
  where
    listAddressesPast b c = runSpec (innSpecPast b c)
    listAddressesFuture pe ta cd = runSpec (innSpecFut pe ta cd)
    rs p f = Map.intersection p f

-- Calls python script that provides input for specific attack and batch
innSpecPast :: Int -> Int -> InnerSpecification Transactions
innSpecPast block count = let
  jsons = unsafePerformIO (getAllJSONs (readproc scriptPath ["--block", show block, "--maxevs", show count]))
  is = IS ins getSwapMap ("false" =: Leaf False) 10
  decs = getDecs is
  ins = map (checkAndConvert (getFromJSONers decs)) jsons
  in is

innSpecFut :: [String] -> [String] -> [String] -> InnerSpecification Transactions
innSpecFut p t a = let
  is = createIS ins getSwapMap ("false" =: Leaf False) 10
  ins = [("paramentry", map toDyn p), ("targetaddress", map toDyn t), ("btokenamt", map toDyn a)]
  in is

-- 3 route is called with execute Now
called3route :: Stream Bool
called3route = "called3route" =:
  -- (Now targetalias === Leaf "3Route v4") && (Now paramentry === Leaf "execute")
  (Now targetaddr === Leaf routeaddress) && (Now paramentry === Leaf "execute")

called3routePrint :: Stream String
called3routePrint = "called3routePrint" =:
  if Now called3route then
    Leaf "called3routeNow"
  else
    Leaf ""

attackers :: Stream (Set.Set String)
attackers = "attackers" =:
  if Now isSandwichAttack then
    Set.insert <$> Now attacker <*> prevSet
  else
    prevSet
  where
    prevSet = attackers :@ (-1, Leaf Set.empty)

fellows :: String -> Stream (Set.Set String)
fellows acc = "fellows" <: acc =:
  Set.insert <$> Now senderaddr <*> prevSet
  where
    prevSet = (fellows acc) :@ (-1, Leaf Set.empty)
  
setSuspicious :: Stream (Set.Set String)
setSuspicious = "setSuspicious" =:
  getSets <$> ((fellows `over` attackers) `withInit` (traceIniter <$> Leaf argmaker) `updating` (Set.singleton <$> Now attacker))
  where
    getSets mp = foldl (Set.union) (Set.empty) (Map.elems mp)
    argmaker att = [scriptPath, "--target", att]


