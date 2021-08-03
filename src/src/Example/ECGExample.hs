{-# LANGUAGE RebindableSyntax  #-}
module Example.ECGExample where
import qualified Prelude as P
import Theories.ECG
import Syntax.HLPrelude
import Syntax.Booleans
import Syntax.Ord
import Syntax.Num
import Lib.Utils
import Lola
import DecDyn

specification :: Specification
specification = [out timestamp, out ecg_measurement, out is_qrs_peak]

-- inputs
ecg_measurement :: Stream Double
ecg_measurement = Input "ecg_measurement"
timestamp :: Stream Int
timestamp = Input "timestamp"
-- bandpass
bandpassed_all :: Stream (Double, (D2, D2))
bandpassed_all = "bandpassed_all" =: let
  butterargs = ((0.16020035, 0, -0.16020035), (1, -1.6795993, 0.6795993)) -- butter(filter_order, [low, high], btype="band")
  in iir_df1 butterargs <$> bp_st :@ (-1, Leaf ((0,0),(0,0))) <*> Now ecg_measurement
bandpassed :: Stream Double
bandpassed = "bandpassed" =: fst <$> Now bandpassed_all
bp_st :: Stream (D2,D2)
bp_st = "bp_st" =: snd <$> Now bandpassed_all
-- stomp_fst_5
stompedbandpassed = "stompedbandpassed" =: 
  if Now instantN <= 5 then stompedbandpassed :@ (-1, bandpassed :@ (5, Leaf undefined)) else Now bandpassed
-- differentiated, squared, convolved
differentiated = "differentiated" =: stompedbandpassed :@ (1,Now stompedbandpassed) - Now stompedbandpassed
squared = "squared" =: (^2) <$> Now differentiated
convolved = "convolved" =: convolved :@ (-1,0) + Now squared - squared :@ (-15,0)
-- findpeaks
-- x = -1.e-6 fifty times, then data, then last(data) - 1.e-6 fifty times
rprev50 = "rprev50" =: shift <$> rprev50 :@ (-1, Leaf $ replicate 50 (-1e-6)) <*> Now convolved
  where shift r x = x:init r
peak_candidate = "peak_candidate" =: let
  nextm50 = convolved :@@ 50
  in dafun <$> Now rprev50 <*> nextm50
  where
  dafun (h_c:rprev49) (_:next49) = h_c P.> 0.35 P.&& all (h_c P.>) (rprev49 ++ next49)
-- QRS
ispeak = Now peak_candidate && Now instantN - Now last_qrs_index > 120
isqrs = Now convolved > Now threshold_value
qrs_peak_value = "qrs_peak_value" =: let
  qrs_peak_filtering_factor = 0.125
  in if ispeak && isqrs then qrs_peak_filtering_factor * Now convolved + (1 - qrs_peak_filtering_factor) * qrs_peak_value :@ (-1,0) else qrs_peak_value :@ (-1,0)
is_qrs_peak = "is_qrs_peak" =: isqrs && ispeak
noise_peak_value = "noise_peak_value" =: let
  noise_peak_filtering_factor = 0.125
  in if ispeak && not isqrs then noise_peak_filtering_factor * Now convolved + (1 - noise_peak_filtering_factor) * noise_peak_value :@ (-1,0) else noise_peak_value :@ (-1,0)
next_threshold_value = "next_threshold_value" =: if ispeak then Now noise_peak_value + 0.25 * (Now qrs_peak_value - Now noise_peak_value) else threshold_value :@ (-1,0)
threshold_value = "threshold_value" =: next_threshold_value :@ (-1, 0)
last_qrs_index = "last_qrs_index" =: next_last_qrs_index :@ (-1, -120)
next_last_qrs_index = "nlqi" =: if ispeak && isqrs then Now instantN else Now last_qrs_index
