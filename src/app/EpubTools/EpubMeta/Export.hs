-- License: ISC (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubMeta.Export
   ( exportOpf )
   where

import Codec.Epub.IO
import System.FilePath

import EpubTools.EpubMeta.Opts
import EpubTools.EpubMeta.Util


write :: FilePath -> String -> EM ()
write path contents = liftIO $ do
   writeFile path contents
   putStrLn $ "OPF XML saved as: " ++ path


exportOpf :: Options -> FilePath -> EM ()
exportOpf opts = export' (optExport opts)


export' :: Export -> FilePath -> EM ()

export' Existing zipPath = do
   (fullPath, contents) <- getPkgPathXmlFromZip zipPath
   write (takeFileName fullPath) contents

export' (ToPath path) zipPath = do
   contents <- getPkgXmlFromZip zipPath
   write path contents

export' NoExport _ = undefined
