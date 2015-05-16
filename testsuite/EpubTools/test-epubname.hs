-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Main
   where

import Control.Monad.Except
import System.Exit
import Test.HUnit ( Counts (..), Test (..), runTestTT )

import EpubTools.EpubName.Format.Format
import EpubTools.EpubName.Main
import EpubTools.EpubName.Opts
import EpubTools.Test.EpubName.Format
import EpubTools.Test.EpubName.PubYear


main :: IO ()
main = do
   dos <- defaultOptions
   let testOpts = dos { optRulesPaths = ["resources/default.rules"] }
   ir <- runExceptT $ initialize testOpts
   either (exitWith) (runTests testOpts) ir
   

runTests :: Options -> [Formatter] -> IO ()
runTests opts fs = do
   counts <- runTestTT $ tests opts fs
   exit $ testsPassed counts


exit :: Bool -> IO ()
exit True  = exitWith ExitSuccess
exit False = exitWith $ ExitFailure 1


testsPassed :: Counts -> Bool
testsPassed (Counts _ _ e f) = (e == 0) && (f == 0)


tests :: Options -> [Formatter] -> Test
tests opts fs = TestList
   [ formatTests opts fs
   , pubYearTests
   ]
