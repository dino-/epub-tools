-- Copyright: 2010-2012 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubMeta.Display
   where

import Codec.Epub.Opf.Format.Package
import Codec.Epub.Opf.Parse

import EpubTools.EpubMeta.Opts
import EpubTools.EpubMeta.Util


display :: Options -> FilePath -> EM ()
display opts zipPath = do
   meta <- parseEpubOpf zipPath
   liftIO $ putStr $ formatPackage (optVerbose opts) meta
