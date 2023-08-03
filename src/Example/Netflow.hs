{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Example.Netflow where
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
import qualified Prelude as P
import qualified Data.Map.Strict as Map
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import DecDyn
import System.IO.Unsafe (unsafePerformIO)
import Example.Attacks
import TraceRetrieving
import System.Environment (getArgs, getExecutablePath)

type Entropy = Map.Map String (Set.Set String)
type Occurrences = Int
type Packets = Int
type Bits = Int
type TimeStart = Int
type TimeEnd = Int
type AddressAccInfo = (Packets, Bits, TimeStart, TimeEnd)
type Histogram = Map.Map String Occurrences
type AddressInfo = Map.Map String AddressAccInfo

scriptPath :: String
scriptPath = let
  binpath = unsafePerformIO getExecutablePath
  localpath = reverse $ dropWhile (/= '/') $ reverse binpath
  in localpath ++ "netFlow/fprocess_netflow.sh"

getPackets :: AddressAccInfo -> Packets
getPackets (p, _, _, _) = p

getBits :: AddressAccInfo -> Bits
getBits (_, b, _, _) = b

getTs :: AddressAccInfo -> TimeStart
getTs (_, _, ts, _) = ts

getTe :: AddressAccInfo -> TimeEnd
getTe (_, _, _, te) = te
  
protocol :: Stream Protocol
protocol = Input "pr"

fileId :: Stream String 
fileId = Input "file"

timeStart :: Stream Int
timeStart = Input "ts"

timeEnd :: Stream Int
timeEnd = Input "te"

destAddr :: Stream String 
destAddr = Input "da"

srcAddr :: Stream String 
srcAddr = Input "sa"

destPort :: Stream String
destPort = Input "dp"

srcPort :: Stream String
srcPort = Input "sp"

flagA :: Stream Bool
flagA = Input "flagA"

flagS :: Stream Bool
flagS = Input "flagS"

flagR :: Stream Bool
flagR = Input "flagR"

flagF :: Stream Bool
flagF = Input "flagF"

flagP :: Stream Bool
flagP = Input "flagP"

flagU :: Stream Bool
flagU = Input "flagU"

flagX :: Stream Bool
flagX = Input "flagX"

bytes :: Stream Int
bytes = Input "ibyt"

pkt :: Stream Int
pkt = Input "ipkt"

duration :: Stream Int
duration = Input "td"

flow :: Stream Flow
flow = "flow" =: Flow <$> Now protocol <*> Now destAddr <*> Now srcAddr <*> Now destPort <*> Now srcPort <*> Now flagA <*> Now flagS <*> Now flagR <*> Now flagF <*> Now flagP <*> Now flagU <*> Now flagX

flowCounter :: Stream Int
flowCounter = "flowCounter" =:
  flowCounter :@ (-1, 0) + 1

newFileId :: Stream Bool
newFileId = "newFileId" =:
  if fileId :@ (-1, Leaf "") /== Now fileId then Leaf True else Leaf False

-- True if the following flow has a different file name
lastFlowFile :: Stream Bool
lastFlowFile = "lastFlowFile" =:
  if fileId :@ (1, Leaf "") /== Now fileId then Leaf True else Leaf False    
  
maxB = maxBound :: Int

  
spec :: Specification
spec = out timeEnd : out lastFlowFile : out fileId : out flowCounter : (map (out . countOut) ad ++ map (out . histSize) ad)

countOut :: AttackData -> Stream String
countOut attData = "countOut" <: attData =:
  if Now (maxOverThreshold attData) then
    if Now (ipEntropyGeneral attData) > Leaf (minEntropy attData) then
      Now (maxDestAddress attData)
    else
      Leaf "Over threshold but not entropy"
  else
    Leaf "No attack"
  
-- Should return TRUE if any marker for that attack is over the threshold
attOverThreshold :: AttackData -> Stream Bool
attOverThreshold attData = "attOverThreshold" <: attData =: let 
  strattacks = map (attackDetection attData) (markers attData)
  exprs = map Now strattacks
  in
    foldl (||) (Leaf False) exprs

maxOverThreshold :: AttackData -> Stream Bool
maxOverThreshold attData = "maxOverThreshold" <: attData =: let 
  strattacks = map (maxAttackDetection attData) (markers attData)
  exprs = map Now strattacks
  in
    foldl (||) (Leaf False) exprs

-- For an attack and a marker, checks if the number of flows is over threshold for current address
attackDetection :: AttackData -> Marker -> Stream Bool
attackDetection attData marker = "attackDetection" <: attData <: marker =:
  if (counter (markerEnum marker)) > Leaf (threshold marker) then
    Leaf True
  else
    Leaf False
  where
    counter BPS = Now (avgBpsCurrentAddr attData)
    counter _ = Now (avgPpsCurrentAddr attData)

-- For an attack and a marker, checks if the number of flows is over threshold for the MAX address
maxAttackDetection :: AttackData -> Marker -> Stream Bool
maxAttackDetection attData marker = "maxAttackDetection" <: attData <: marker =:
  if (counter (markerEnum marker)) > Leaf (threshold marker) then
    Leaf True
  else
    Leaf False
  where
    counter BPS = Now (avgBpsMaxAddr attData)
    counter _ = Now (avgPpsMaxAddr attData)

-- Histogram -> map with dest_ips and how many (flows, packets, bytes) filtered by that attack had that IP as dest
attackData :: AttackData -> Stream AddressInfo
attackData attData = "attackData" <: attData =:
  if filt attData <$> Now flow then 
    updateAddress <$> Now destAddr <*> hist <*> (createTuple <$> Now pkt <*> Now bytes <*> Now timeStart <*> Now timeEnd)
  else
    hist
  where
    updateValue (p,b,ts,te) (p',b',ts',te') = let
      !c1 = p+p'
      !c2 = b+b'
      !c3 = min ts ts'
      !c4 = max te te'
      in (c1,c2,c3,c4)
    hist = if Now newFileId then Leaf Map.empty else attackData attData :@ (-1, Leaf Map.empty)     
    updateAddress addr histo val = Map.insertWith (updateValue) addr val histo
    createTuple p b ts te = (p, 8 * b, ts, te)

attackHist :: AttackData -> Stream Histogram
attackHist attData = "attackHist" <: attData =:
  if filt attData <$> Now flow then 
    updateAddress <$> Now destAddr <*> hist <*> 1
  else
    hist
  where
    hist = if Now newFileId then Leaf Map.empty else attackHist attData :@ (-1, Leaf Map.empty)     
    updateAddress addr histo val  = Map.insertWith (+) addr val histo

histSize :: AttackData -> Stream Int
histSize attData = "histSize" <: attData =:
  Map.size <$> Now (attackHist attData)

maybeAddress :: AttackData -> Stream (Set.Set String)
maybeAddress attData = "maybeAddress" <: attData =:
  if Now newFileId then
    Leaf Set.empty
  else
    if Now (maxOverThreshold attData) then
      Set.singleton <$> (Now (maxDestAddress attData))
    else
      Leaf Set.empty
    
-- For an attack, the address which has received most flows during the batch
maxDestAddress :: AttackData -> Stream String
maxDestAddress attData = "maxDestAddress" <: attData =:
  if occurrencesCurrent > occurrencesPrev then currentAddr else previousMaxAddr
  where
    occurrencesCurrent = Map.findWithDefault 0 <$> currentAddr <*> currentHist
    occurrencesPrev = Map.findWithDefault 0 <$> previousMaxAddr <*> currentHist
    previousMaxAddr = maxDestAddress attData :@ (-1, Leaf "")
    currentAddr = Now destAddr
    currentHist = Now (attackHist attData)
    
ipEntropyGeneral :: AttackData -> Stream Int
ipEntropyGeneral
  | elem "--over" (unsafePerformIO getArgs) = ipEntropyOver
  | otherwise = ipEntropy
  
-- For an attack, the number of different source addresses for the max dest ip
ipEntropy :: AttackData -> Stream Int
ipEntropy attData = "ipEntropy" <: attData =:
  Set.size <$> (findMember <$> Now (maxDestAddress attData) <*> ipEntropyAllAddr attData :@(-1, Leaf Map.empty))
  -- Set.size <$> (findMember <$> Now (maxDestAddress attData) <*> ipEntropyAllAddrHotover attData :@(-1, Leaf Map.empty))
  where
    findMember addr entropy = Map.findWithDefault (Set.empty) addr entropy

ipEntropyOver :: AttackData -> Stream Int
ipEntropyOver attData = "ipEntropyOver" <: attData =:
  if (Map.null <$> overMap) then
    0
  else
    maybe 0 Set.size . listToMaybe . Map.elems <$> overMap
  where
    att_name = name attData ++ suffix (markerEnum $ head $ markers attData)
    overMap = setSrcForDestAddr attData `over` maybeAddress attData `withInit` (traceIniter <$> (argmaker <$> Now fileId <*> Now flowCounter))
    argmaker fileid fl _ = [scriptPath, "--flows", "--date", fileid, "--attack",  att_name, "--until", show fl]
   
setDestAddresses :: AttackData -> Stream (Set.Set String)
setDestAddresses attData = "setDestAddresses" <: attData =:
  Set.insert <$> Now destAddr <*> prevSet
  where
    prevSet =
      if Now newFileId then
        Leaf Set.empty
      else
        setDestAddresses attData :@(-1, Leaf Set.empty)

setSrcForDestAddr :: AttackData -> String -> Stream (Set.Set String)
setSrcForDestAddr attData k_a = "setSrcForDestAddr" <: k_a =:
  if (Now destAddr === Leaf k_a) then
    Set.insert <$> Now srcAddr <*> prevSet
  else
    prevSet
  where
    prevSet =
      if Now newFileId then
        Leaf Set.empty
      else
        setSrcForDestAddr attData k_a :@(-1, Leaf Set.empty)

-- Expr (Map x a) --> Map String Set(String)
ipEntropyAllAddrHotover :: AttackData -> Stream Entropy
ipEntropyAllAddrHotover attData = "ipEntropyAllAddrHotover" <: attData =:
  setSrcForDestAddr attData `over` setDestAddresses attData `updating` (Set.singleton <$> Now destAddr)

ipEntropyAllAddr :: AttackData -> Stream Entropy
ipEntropyAllAddr attData = "ipEntropyAllAddr" <: attData =:
  if isMember <$> Now destAddr <*> prevEntropy then
    updateEntropy <$> Now srcAddr <*>  Now destAddr <*> prevEntropy
  else
    mapInsert <$> Now destAddr <*> (singletonSet <$> Now srcAddr) <*> prevEntropy
  where
    singletonSet addr = Set.singleton addr
    isMember addr map = Map.member addr map
    insert addr set = Set.insert addr set
    updateEntropy addrValue addrKey map = Map.adjust (insert addrValue) addrKey map
    mapInsert addr set map = Map.insert addr set map
    prevEntropy =
      if Now newFileId then
        Leaf Map.empty
      else 
        ipEntropyAllAddr attData :@ (-1, Leaf Map.empty)

divOrZero a 0 = 0
divOrZero a b = div a b

-- Average pps of max addr: Find address that received the most flows, and
-- for that address, returns accumulated PACKETS / BATCH DURATION (in sec)
-- (ideally time between first and last flow instead of batch duration)
avgPpsMaxAddr :: AttackData -> Stream Int
avgPpsMaxAddr attData = "avgPpsMaxAddr" <: attData =:
  if timeDur === 0 then
    0
  else
    divOrZero <$> Now (pktMaxAddr attData) <*> timeDur
  where
    timeDur = Now (teMaxAddr attData) - Now (tsMaxAddr attData)

-- Average pps of CURRENT dest addr: For current dest address
-- return accumulated PACKETS / BATCH DURATION (in sec)
-- (ideally time between first and last flow instead of batch duration)
avgPpsCurrentAddr :: AttackData -> Stream Int
avgPpsCurrentAddr attData = "avgPpsCurrentAddr" <: attData =:
  if timeDur === 0 then
    0
  else
    divOrZero <$> Now (pktCurrentAddr attData) <*> timeDur
  where
    timeDur = Now (teCurrentAddr attData) - Now (tsCurrentAddr attData)      

avgBpsMaxAddr :: AttackData -> Stream Int
avgBpsMaxAddr attData = "avgBpsMaxAddr" <: attData =:
  if timeDur === 0 then
    0
  else
    divOrZero <$> Now (bitsMaxAddr attData) <*> timeDur
  where
    timeDur = Now (teMaxAddr attData) - Now (tsMaxAddr attData)

avgBpsCurrentAddr :: AttackData -> Stream Int
avgBpsCurrentAddr attData = "avgBpsCurrentAddr" <: attData =:
  if timeDur === 0 then
    0
  else
    divOrZero <$> Now (bitsCurrentAddr attData) <*> timeDur
  where
    timeDur = Now (teCurrentAddr attData) - Now (tsCurrentAddr attData)     

-- Accumulated pkt of the most used dst address  
pktMaxAddr :: AttackData -> Stream Int
pktMaxAddr attData = "pktMaxAddr" <: attData =:
  getPackets <$> (getAddressData <$> Now (maxDestAddress attData) <*> Now (attackData attData))

-- Accumulated pkt of the CURRENT dst address  
pktCurrentAddr :: AttackData -> Stream Int
pktCurrentAddr attData = "pktCurrentAddr" <: attData =:
  getPackets <$> (getAddressData <$> Now destAddr <*> Now (attackData attData))  

-- Accumulated pps of the most used dst address  
bitsMaxAddr :: AttackData -> Stream Int
bitsMaxAddr attData = "bitsMaxAddr" <: attData =:
  getBits <$> (getAddressData <$> Now (maxDestAddress attData) <*> Now (attackData attData))

-- Accumulated pps of the CURRENT dst address  
bitsCurrentAddr :: AttackData -> Stream Int
bitsCurrentAddr attData = "bitsCurrentAddr" <: attData =:
  getBits <$> (getAddressData <$> Now destAddr <*> Now (attackData attData))  

-- Minimum starting time of the most used dst address  
tsMaxAddr :: AttackData -> Stream Int
tsMaxAddr attData = "tsMaxAddr" <: attData =:
  getTs <$> (getAddressData <$> Now (maxDestAddress attData) <*> Now (attackData attData))

-- Minimum starting time of the CURRENT dst address  
tsCurrentAddr :: AttackData -> Stream Int
tsCurrentAddr attData = "tsCurrentAddr" <: attData =:
  getTs <$> (getAddressData <$> Now destAddr <*> Now (attackData attData))  

-- Minimum starting time of the most used dst address  
teMaxAddr :: AttackData -> Stream Int
teMaxAddr attData = "teMaxAddr" <: attData =:
  getTe <$> (getAddressData <$> Now (maxDestAddress attData) <*> Now (attackData attData))

-- Minimum starting time of the CURRENT dst address  
teCurrentAddr :: AttackData -> Stream Int
teCurrentAddr attData = "teCurrentAddr" <: attData =:
  getTe <$> (getAddressData <$> Now destAddr <*> Now (attackData attData))      

-- Get the value for the input key address in histogram
getAddressData :: String  -> AddressInfo -> AddressAccInfo
getAddressData addr hist = Map.findWithDefault (0,0,0,0) addr hist

-- Calls python script that provides input for specific attack and batch
innSpec :: AttackData -> String -> String -> InnerSpecification String
innSpec attackData attack_name fileId = let
  jsons = unsafePerformIO (getAllJSONs (readproc scriptPath ["--flows", "--date", fileId, "--attack", attack_name]))
  is = IS ins (countOut attackData) ("false" =: Leaf False) 10
  decs = getDecs is
  ins = map (checkAndConvert (getFromJSONers decs)) jsons
  in is

underAttack :: AttackData -> Stream String
underAttack attData = "underAttack" <: attData =:
  if Now newFileId then
    if Now (ipEntropyOver attData) > Leaf (minEntropy attData) then
      Now (maxDestAddress attData)
    else
      Leaf "Over threshold but not entropy"
  else
    Leaf "Not new file" 

underAttackDuration :: AttackData -> Stream String
underAttackDuration attData = "underAttackDuration" <: attData =:  
  if Now (ipEntropy attData) > Leaf (minEntropy attData) then
    makeString <$> Now (maxDestAddress attData) <*> timeDur
  else
    Leaf "Over threshold but not entropy"
  where
    timeDur = Now (teMaxAddr attData) - Now (tsMaxAddr attData)
    makeString s d = s ++ ", duration: " ++ show d    
