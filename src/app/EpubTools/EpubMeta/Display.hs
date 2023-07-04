-- License: ISC (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubMeta.Display
   where

import Codec.Epub
import Control.Monad ( when )

import EpubTools.EpubMeta.Opts
import EpubTools.EpubMeta.Util


display :: Options -> FilePath -> EM ()
display opts zipPath = do
   xml <- getPkgXmlFromZip zipPath

   pkg <- format `fmap` getPackage xml
   meta <- format `fmap` getMetadata xml
   liftIO $ mapM_ putStr [pkg, meta]

   when (optVerbose opts) $ do
      mf <- format `fmap` getManifest xml
      spine <- format `fmap` getSpine xml
      guide <- format `fmap` getGuide xml
      liftIO $ mapM_ putStr [mf, spine, guide]
