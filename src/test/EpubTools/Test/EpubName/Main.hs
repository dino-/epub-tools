import Control.Monad.Except (runExceptT)
import System.Exit (exitWith)
import Test.Tasty (defaultMain, testGroup)

import EpubTools.EpubName.Common (Options, defaultOptions)
import EpubTools.EpubName.Format.Format (Formatter)
import EpubTools.EpubName.Main (initialize)
import EpubTools.Test.EpubName.Format (formatTests)
import EpubTools.Test.EpubName.PubYear (pubYearTests)


main :: IO ()
main = do
   -- Can use this def to adjust the options with defaultOptions { ... }
   let testOpts = defaultOptions
   ir <- runExceptT $ initialize testOpts
   either (exitWith) (runTests testOpts) ir
   

runTests :: Options -> [Formatter] -> IO ()
runTests opts fs = defaultMain $ testGroup " EpubName tests"
   [ formatTests opts fs
   , pubYearTests
   ]
