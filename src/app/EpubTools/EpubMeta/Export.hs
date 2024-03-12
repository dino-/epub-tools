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


exportOpf :: Output -> FilePath -> EM ()

exportOpf (OutputFilename path) zipPath = do
   contents <- getPkgXmlFromZip zipPath
   write path contents

exportOpf ExistingName zipPath = do
   (fullPath, contents) <- getPkgPathXmlFromZip zipPath
   write (takeFileName fullPath) contents

exportOpf StdOut zipPath = do
   (_, contents) <- getPkgPathXmlFromZip zipPath
   liftIO $ putStrLn contents
