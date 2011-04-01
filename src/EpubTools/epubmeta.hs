-- Copyright: 2010, 2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Monad
import System.Environment ( getArgs )
import System.Exit

import EpubTools.EpubMeta.Display
import EpubTools.EpubMeta.Opts


main :: IO ()
main = do
   (opts, paths) <- getArgs >>= parseOpts

   when ((optHelp opts) || (null paths)) $ do
      putStrLn usageText
      exitWith $ ExitFailure 1

   let zipPath = head paths
   displayMeta opts zipPath
