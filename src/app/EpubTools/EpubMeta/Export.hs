module EpubTools.EpubMeta.Export
  ( exportOpf )
  where

import Codec.Epub.IO (getPkgPathXmlFromZip, getPkgXmlFromZip)
import System.FilePath (takeFileName)

import EpubTools.EpubMeta.Opts (EpubPath (..),
  Output (ExistingName, OutputFilename, StdOut))
import EpubTools.EpubMeta.Util (EM, liftIO)


write :: FilePath -> String -> EM ()
write path contents = liftIO $ do
  writeFile path contents
  putStrLn $ "OPF XML saved as: " ++ path


exportOpf :: Output -> EpubPath -> EM ()

exportOpf (OutputFilename path) (EpubPath zipPath) = do
  contents <- getPkgXmlFromZip zipPath
  write path contents

exportOpf ExistingName (EpubPath zipPath) = do
  (fullPath, contents) <- getPkgPathXmlFromZip zipPath
  write (takeFileName fullPath) contents

exportOpf StdOut (EpubPath zipPath) = do
  (_, contents) <- getPkgPathXmlFromZip zipPath
  liftIO $ putStrLn contents
