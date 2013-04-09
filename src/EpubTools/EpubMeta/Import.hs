-- Copyright: 2010-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubMeta.Import
   ( importOpf )
   where

import Codec.Archive.Zip ( Entry (..), addEntryToArchive, readEntry, toArchive )
import Codec.Epub2.Archive ( writeArchive )
import Codec.Epub2.IO ( opfContentsFromBS )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
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
   strictBytes <- liftIO $ BS.readFile zipPath
   (pathToOldFile, _) <- opfContentsFromBS strictBytes

   -- Adjust the entry's path
   let newEntry = tempEntry { eRelativePath = pathToOldFile }

   -- Bytes of existing archive already loaded, make an Archive from that
   let oldArchive = toArchive . BL.fromChunks $ [strictBytes]

   -- Add new entry to it (replacing old one)
   let newArchive = addEntryToArchive newEntry oldArchive

   -- We're ready to write the new file, delete the old one
   liftIO $ removeFile zipPath

   -- Write the new archive out
   liftIO $ writeArchive zipPath newArchive
