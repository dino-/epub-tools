-- License: ISC (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE ScopedTypeVariables #-}

module EpubTools.EpubMeta.Edit
   ( edit )
   where

import Control.Exception
import Control.Monad
import System.Directory
import System.FilePath
import System.Environment
import System.Exit
import System.Process ( system )
import Text.Printf

import EpubTools.EpubMeta.Export
import EpubTools.EpubMeta.Import
import EpubTools.EpubMeta.Opts
import EpubTools.EpubMeta.Util


{- Turn an (IO a) action into a (Maybe a) with a value of Nothing
   if an IO exception is raised
-}
ioMaybe :: IO a -> IO (Maybe a)
ioMaybe action = do
   result :: (Either SomeException a) <- try action
   either (const . return $ Nothing) (return . Just) result


{- Look through the user's environment looking for their preferred 
   editor. Use /usr/bin/vi as a default if none found
-}
findEditor :: EM FilePath
findEditor = do
   mbEditor <- liftIO $ foldr (liftM2 mplus) (findExecutable "vi")
      [ ioMaybe $ getEnv "EDITOR"
      , ioMaybe $ getEnv "VISUAL"
      ]

   maybe (throwError "epubmeta: ERROR: Could not find a suitable editor in your EDITOR or VISUAL environment variables, and could not find the vi binary. Fix this situation or use epubmeta export/import.") return mbEditor


edit :: Options -> FilePath -> EM ()
edit opts = edit' $ optEdit opts


edit' :: Edit -> FilePath -> EM ()

edit' NoBackup zipPath = do
   -- Make a temp path for the OPF document
   tempDir <- liftIO $ getTemporaryDirectory
   let opfPath = tempDir </> "epubmeta-temp-content.opf"

   -- Dump the OPF document to this path
   exportOpf (defaultOptions { optExport = ToPath opfPath }) zipPath

   -- Open this file in the user's editor and wait for it
   editor <- findEditor
   ec <- liftIO $ system $ printf "%s %s" editor opfPath

   case ec of
      ExitSuccess -> do
         -- Modify the book with the now-edited OPF data
         importOpf (defaultOptions { optImport = Just opfPath }) zipPath

         -- Delete the temp file
         liftIO $ removeFile opfPath

      ExitFailure _ -> do
         -- Delete the temp file
         liftIO $ removeFile opfPath

         throwError "epubmeta: ERROR: Something went wrong during editing, your file has not been changed."

edit' (Backup suffix) zipPath = do
   liftIO $ copyFile zipPath (zipPath ++ suffix)
   edit' NoBackup zipPath

edit' NotEditing _ = undefined
