{-# LANGUAGE ScopedTypeVariables #-}

module EpubTools.EpubMeta.Edit
  ( edit )
  where

import Control.Exception (SomeException, try)
import Data.Monoid (First (..), getFirst)
import System.Directory (copyFile, findExecutable, getTemporaryDirectory,
  removeFile)
import System.FilePath ((</>))
import System.Environment (getEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (system)
import Text.Printf (printf)

import EpubTools.EpubMeta.Export (exportOpf)
import EpubTools.EpubMeta.Import (importOpf)
import EpubTools.EpubMeta.Opts (Backup (BackupSuffix, NoBackup),
  EpubPath (..), ImportPath (..), Output (OutputFilename))
import EpubTools.EpubMeta.Util (EM, liftIO, throwError)


{- Turn an (IO a) action into a (Maybe a) with a value of Nothing
   if an IO exception is raised
-}
ioMaybe :: IO a -> IO (Maybe a)
ioMaybe action = do
  result :: (Either SomeException a) <- try action
  either (const . return $ Nothing) (return . Just) result


{- Look through the user's environment for their preferred editor.
   Use vi wherever it may be on the PATH as a default if none found.
-}
findEditor :: EM FilePath
findEditor = do
  mbEditor <- liftIO $ (getFirst . mconcat . map First) <$> sequence
    [ ioMaybe $ getEnv "EDITOR"
    , ioMaybe $ getEnv "VISUAL"
    , findExecutable "vi"
    ]

  maybe (throwError "epubmeta: ERROR: Could not find a suitable editor in your EDITOR or VISUAL environment variables, and could not find the vi binary. Fix this situation or use epubmeta export/import.") return mbEditor


edit :: Backup -> EpubPath -> EM ()

edit NoBackup epubPath = do
  -- Make a temp path for the OPF document
  tempDir <- liftIO $ getTemporaryDirectory
  let opfPath = tempDir </> "epubmeta-temp-content.opf"

  -- Dump the OPF document to this path
  exportOpf (OutputFilename opfPath) epubPath

  -- Open this file in the user's editor and wait for it
  editor <- findEditor
  ec <- liftIO $ system $ printf "%s %s" editor opfPath

  case ec of
    ExitSuccess -> do
      -- Modify the book with the now-edited OPF data
      importOpf (ImportPath opfPath) NoBackup epubPath

      -- Delete the temp file
      liftIO $ removeFile opfPath

    ExitFailure _ -> do
      -- Delete the temp file
      liftIO $ removeFile opfPath

      throwError "epubmeta: ERROR: Something went wrong during editing, your file has not been changed."

edit (BackupSuffix suffix) epubPath@(EpubPath zipPath) = do
  liftIO $ copyFile zipPath (zipPath ++ suffix)
  edit NoBackup epubPath
