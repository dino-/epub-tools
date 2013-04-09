-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubMeta.Export
   ( exportOpf )
   where

import Codec.Epub2.IO
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
   (fullPath, contents) <- opfContentsFromZip zipPath
   write (takeFileName fullPath) contents

export' (ToPath path) zipPath = do
   (_, contents) <- opfContentsFromZip zipPath
   write path contents

export' NoExport _ = undefined
