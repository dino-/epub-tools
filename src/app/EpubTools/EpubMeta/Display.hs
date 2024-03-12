module EpubTools.EpubMeta.Display
   where

import Codec.Epub
import Control.Monad ( when )

import EpubTools.EpubMeta.Opts
import EpubTools.EpubMeta.Util


display :: Verbose -> FilePath -> EM ()
display (Verbose verbose) zipPath = do
   xml <- getPkgXmlFromZip zipPath

   pkg <- format `fmap` getPackage xml
   meta <- format `fmap` getMetadata xml
   liftIO $ mapM_ putStr [pkg, meta]

   when verbose $ do
      mf <- format `fmap` getManifest xml
      spine <- format `fmap` getSpine xml
      guide <- format `fmap` getGuide xml
      liftIO $ mapM_ putStr [mf, spine, guide]
