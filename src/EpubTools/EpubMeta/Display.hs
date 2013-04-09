-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubMeta.Display
   where

import Codec.Epub2.Opf.Format.Package
import Codec.Epub2.Opf.Parse

import EpubTools.EpubMeta.Opts
import EpubTools.EpubMeta.Util


display :: Options -> FilePath -> EM ()
display opts zipPath = do
   meta <- parseEpub2Opf zipPath
   liftIO $ putStr $ formatPackage (optVerbose opts) meta
