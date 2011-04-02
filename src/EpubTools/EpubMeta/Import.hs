-- Copyright: 2010, 2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubMeta.Import
   ( importOpf )
   where

import Codec.Archive.Zip ( Entry (..), addEntryToArchive, readEntry )
import Codec.Epub.Archive ( readArchive, writeArchive )
import Codec.Epub.IO ( opfContentsFromZip )
import Data.Maybe ( fromJust )
import System.Directory ( removeFile )

import EpubTools.EpubMeta.Opts
import EpubTools.EpubMeta.Util


importOpf :: Options -> FilePath -> EM ()
importOpf opts zipPath = do
   -- The file we want to insert into the book
   let pathToNewOpf = fromJust . optImport $ opts

   -- Make new file into a Zip Entry
   tempEntry <- liftIO $ readEntry [] pathToNewOpf

   -- The path in the book where new file must go
   (pathToOldFile, _) <- opfContentsFromZip zipPath

   -- Adjust the entry's path
   let newEntry = tempEntry { eRelativePath = pathToOldFile }

   -- Load existing archive
   oldArchive <- liftIO $ readArchive zipPath

   -- Add new entry to it (replacing old one)
   let newArchive = addEntryToArchive newEntry oldArchive

   -- Breaks the lock on the old file
   liftIO $ removeFile zipPath

   -- Write the new archive out
   liftIO $ writeArchive zipPath newArchive
