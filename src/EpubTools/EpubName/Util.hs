-- Copyright: 2008-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubName.Util
   where

import System.Exit


exitInitFailure, exitProcessingFailure :: ExitCode
exitInitFailure = ExitFailure 1
exitProcessingFailure = ExitFailure 128
