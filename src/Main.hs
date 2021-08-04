{-# LANGUAGE RebindableSyntax  #-}
module Main where
import qualified Example.Accum as Accum (spec)
import InFromFile
import System.IO
import Lola
import System.Environment
import Prelude
import qualified Prelude as P
import DecDyn

importedSpec :: Specification
importedSpec = Accum.spec

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  runSpecCSV False importedSpec
