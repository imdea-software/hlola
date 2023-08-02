{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
module INNERSPECSDIR.INNERSPEC_flowAnalyzer where
import Lola
import GHC.Generics
import Data.Aeson
import Syntax.HLPrelude
import DecDyn (InnerSpecification(),createIS, bind)
import Syntax.Booleans
import Syntax.Ord
import Syntax.Num
import qualified Prelude as P
import Theories.Attacks
import Data.Set hiding (empty,size,elems)
import qualified Data.Set (size,empty)
import Data.Map.Strict hiding (union,singleton,insert)
import Data.Maybe
import Lib.DynPar


-- Custom datas

type Entropy = Map String (Set String)
type Histogram = Map String Int
type AddressInfo = Map String (Int, Int, Int)

-- Custom Haskell


flowAnalyzer ::  AttackData -> String -> [Flow] -> InnerSpecification String
flowAnalyzer atk fileId flow__arg = createIS [bind flow flow__arg] attacked_addr falsestr (5)
  where
  getMarker (x,_,_) = x
  getTS (_,x,_) = x
  getTE (_,_,x) = x
  emptySet = Data.Set.empty
  setSize = Data.Set.size

  flow :: Stream Flow
  flow = Input $ "flow" 

  flowCounter :: Stream Int
  flowCounter = "flowCounter" =: ((flowCounter:@(-1,0))  + 1)

  destAddr :: Stream String
  destAddr = "destAddr" =: ((toolLift dA)  (flow:@(0, Leaf undefined)))

  srcAddr :: Stream String
  srcAddr = "srcAddr" =: ((toolLift sA)  (flow:@(0, Leaf undefined)))

  attacked_addr :: Stream String
  attacked_addr = "attacked_addr" =: (
    if  (attack_detection:@(0, Leaf undefined))  then
       (maxDestAddress:@(0, Leaf undefined)) 
    else (toolLift ("No attack")) )

  attack_detection :: Stream Bool
  attack_detection = "attack_detection" =: (
     (markerRate:@(0, Leaf undefined))  > (toolLift (threshold atk))  &&
     (ipEntropy:@(0, Leaf undefined))  > (toolLift (maxEntropy atk)) )

  markerRate :: Stream Int
  markerRate = "markerRate" =: (
    if timeDur === 0 then 0
    else (magic2 div) ((magic1 getMarker) addrData) timeDur)
    where
      timeDur = (toolLift getTE) addrData - (toolLift getTS) addrData
      addrData = (toolLift (!))   (addrInfo:@(0, Leaf undefined))   (maxDestAddress:@(0, Leaf undefined))

  maxDestAddress :: Stream String
  maxDestAddress = "maxDestAddress" =: (
    if occurrencesCurrent > occurrencesPrev
    then currentAddr else previousMaxAddr)
    where
      currentAddr =  (destAddr:@(0, Leaf undefined)) 
      currentHist =  (attackHist:@(0, Leaf undefined)) 
      occurrencesCurrent = (toolLift (!))  currentHist currentAddr
      occurrencesPrev = (toolLift (!))  currentHist previousMaxAddr
      previousMaxAddr =  (maxDestAddress:@(-1,(toolLift ""))) 

  addrInfo :: Stream AddressInfo
  addrInfo = "addrInfo" =: (
    (magic3 (insertWith updateValue))   (destAddr:@(0, Leaf undefined))  ((toolLift (getInfo (markerEnum (head (markers atk)))))   (flow:@(0, Leaf undefined)) ) prev)
    where
      prev =  (addrInfo:@(-1,(toolLift empty))) 
      updateValue (m,ts,te) (m',ts',te') = (m+m',min ts ts',max te te')
      getInfo BPS f = (bts f * 8, ts f, te f)
      getInfo PPS f = (pkts f * 8, ts f, te f)

  attackHist :: Stream Histogram
  attackHist = "attackHist" =: (
    (magic3 (insertWith (+)))   (destAddr:@(0, Leaf undefined))  1 hist)
    where
      hist =  (attackHist:@(-1,(toolLift empty))) 

  ipEntropy :: Stream Int
  ipEntropy = "ipEntropy" =: (
    (toolLift (maybe 0 setSize . listToMaybe . elems))  mset)
    where
      mset = setSrcForDestAddr
             `over` maybeAddress
             `withInit` (magic1 (initer atk fileId))   (flowCounter:@(0, Leaf undefined))

  setSrcForDestAddr :: String -> Stream (Set String)
  setSrcForDestAddr dst = "setSrcForDestAddr" <: dst =: (
    (toolLift insert)  (srcAddr:@(0, Leaf undefined))  prevSet)
    where
      prevSet =  ((setSrcForDestAddr dst):@(-1,(toolLift emptySet))) 

  maybeAddress :: Stream (Set String)
  maybeAddress = "maybeAddress" =: (
    if  (attack_detection:@(0, Leaf undefined)) 
    then (toolLift singleton)  (maxDestAddress:@(0, Leaf undefined)) 
    else (toolLift emptySet) )

  falsestr :: Stream Bool
  falsestr = "falsestr" =: ((toolLift False) )
