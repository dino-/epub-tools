module EpubTools.EpubName.Util
   where

import System.Exit


exitInitFailure, exitProcessingFailure :: ExitCode
exitInitFailure = ExitFailure 1
exitProcessingFailure = ExitFailure 128
