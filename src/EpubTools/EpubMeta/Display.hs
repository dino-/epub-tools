-- Copyright: 2010, 2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubMeta.Display
   where

import Codec.Epub.Opf.Format.Package
import Codec.Epub.Opf.Parse
import Control.Monad.Error

import EpubTools.EpubMeta.Opts


displayMeta :: Options -> FilePath -> IO ()
displayMeta opts zipPath = do
   result <- runErrorT $ parseEpubOpf zipPath

   putStr $ either id (formatPackage (optVerbose opts)) result
