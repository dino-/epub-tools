module EpubTools.EpubMeta.Import
  ( importOpf )
  where

import Codec.Archive.Zip (Entry (..), addEntryToArchive, findEntryByPath,
  readEntry, toArchive)
import Codec.Epub.IO ( getPkgPathXmlFromBS, writeArchive )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List (foldl')
import System.Directory (copyFile, removeFile)

import EpubTools.EpubMeta.Opts (Backup (BackupSuffix, NoBackup),
  EpubPath (..), ImportPath (..))
import EpubTools.EpubMeta.Util (EM, liftIO, throwError)


importOpf :: ImportPath -> Backup -> EpubPath -> EM ()

importOpf (ImportPath pathToNewOpf) NoBackup (EpubPath zipPath) = do
  -- Make new file into a Zip Entry
  tempEntry <- liftIO $ readEntry [] pathToNewOpf

  -- Read the zip archive in, we'll need it in several steps
  strictBytes <- liftIO $ BS.readFile zipPath

  -- The path in the book where new file must go
  (pathToOldFile, _) <- getPkgPathXmlFromBS strictBytes

  -- Adjust the entry's path
  let newOpfEntry = tempEntry { eRelativePath = pathToOldFile }

  -- Bytes of existing archive already loaded, make an Archive from that
  let oldArchive = toArchive . BL.fromChunks $ [strictBytes]

  -- Make an entry from the existing mimetype file
  mimeEntry <- maybe
    (throwError "Something went wrong: This epub has no mimetype, which shouldn't be possible. Original file unchanged.")
    return $ findEntryByPath "mimetype" oldArchive

  -- Add the two entries back to the archive in this order to preserve the
  -- mimetype as the first one
  let newArchive = foldl' (flip id) oldArchive
        [addEntryToArchive newOpfEntry, addEntryToArchive mimeEntry]

  -- We're ready to write the new zip archive, delete the old one
  liftIO $ removeFile zipPath

  -- Write the new archive out
  liftIO $ writeArchive zipPath newArchive

importOpf importPath (BackupSuffix suffix) epubPath@(EpubPath zipPath) = do
  liftIO $ copyFile zipPath (zipPath ++ suffix)
  importOpf importPath NoBackup epubPath
