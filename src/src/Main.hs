{-# LANGUAGE RebindableSyntax  #-}
module Main where
import InFromFile
import System.IO
import Lola
import System.Environment
import Prelude
import Lib.DynMTL (until)
import qualified Prelude as P
import DecDyn

main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs [ws] = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  runSpecCSV False (specification (read ws))
parseArgs _ = putStrLn "Wrong arguments. Check spec."

specification ws = [out property]
  where
  phi = Input "phi"
  psi = Input "psi"
  property = Lib.DynMTL.until (0,Leaf ws) phi psi
