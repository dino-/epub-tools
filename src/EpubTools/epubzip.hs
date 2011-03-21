-- Copyright: 2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Codec.Epub.Archive
import Control.Monad
import System.Environment
import System.Exit

import EpubTools.EpubZip.Opts


main :: IO ()
main = do
   (opts, paths) <- getArgs >>= parseOpts

   when ((optHelp opts) || (null paths)) $ do
      putStrLn usageText
      exitWith $ ExitFailure 1

   let zipPath = head paths

   archive <- mkEpubArchive "."

   writeArchive zipPath archive
