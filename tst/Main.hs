module Main
  (main) where

import qualified Math.ArbitraryReal
  as MAR
import qualified Math.ArbitraryLRT
  as MALRT
import Control.Monad
  (unless)
import Test.QuickCheck
  (isSuccess)
import System.Exit
  (exitFailure)

main :: IO ()
main = do
  mar <- MAR.runRealTests
  malrt <- MALRT.runTests
  
  unless (and [mar, malrt])
    exitFailure
