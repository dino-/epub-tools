module EpubTools.EpubMeta.View
  ( view )
  where

import Codec.Epub (format, getGuide, getManifest, getMetadata, getPackage,
  getPkgXmlFromZip, getSpine)
import Control.Monad (when)

import EpubTools.EpubMeta.Opts (EpubPath (..), Verbose (..))
import EpubTools.EpubMeta.Util (EM, liftIO)


view :: Verbose -> EpubPath -> EM ()
view (Verbose verbose) (EpubPath zipPath) = do
  xml <- getPkgXmlFromZip zipPath

  pkg <- format `fmap` getPackage xml
  meta <- format `fmap` getMetadata xml
  liftIO $ mapM_ putStr [pkg, meta]

  when verbose $ do
    mf <- format `fmap` getManifest xml
    spine <- format `fmap` getSpine xml
    guide <- format `fmap` getGuide xml
    liftIO $ mapM_ putStr [mf, spine, guide]
