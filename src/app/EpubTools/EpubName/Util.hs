module EpubTools.EpubName.Util
  ( exitInitFailure
  , exitProcessingFailure
  )
  where

import System.Exit (ExitCode (ExitFailure))


exitInitFailure, exitProcessingFailure :: ExitCode
exitInitFailure = ExitFailure 1
exitProcessingFailure = ExitFailure 128
