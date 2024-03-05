-- This is for GHC 7.8/7.10 compatibility with the
-- Control.Applicative import below
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE FlexibleContexts, OverloadedRecordDot #-}

import Codec.Epub (format, getMetadata, getPackage, getPkgXmlFromZip)
import Codec.Epub.Data.Metadata (Metadata)
import Codec.Epub.Data.Package (Package)
import Control.Applicative
import Control.Exception (try)
import Control.Monad.Except (MonadIO, liftIO, runExceptT, when)
import Data.List.NonEmpty (toList)
import GHC.IO.Exception (IOException)
import System.Directory (copyFile, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import System.Exit (ExitCode (ExitSuccess), exitWith)
import System.IO ( BufferMode ( NoBuffering )
                 , hSetBuffering, stdin, stdout, stderr 
                 )
import System.Posix (createLink, removeLink)
import Text.Printf (printf)

import EpubTools.EpubName.Common
  ( BookFiles (..)
  , InteractiveSwitch (..)
  , MoveSwitch (..)
  , NoActionSwitch (..)
  , Options (bookFiles, interactive, move, noAction, targetDirs, verbosityLevel)
  , TargetDirs (..)
  , VerbosityLevel (Normal, ShowFormatter, ShowBookInfo)
  )
import qualified EpubTools.EpubName.Common (VerbosityLevel (Normal, ShowBookInfo, ShowFormatter))
import EpubTools.EpubName.Format.Format ( Formatter (..), tryFormatting )
import EpubTools.EpubName.Format.Util (Globals (..), throwError)
import EpubTools.EpubName.Main (initialize)
import EpubTools.EpubName.Opts (parseOpts)
import EpubTools.EpubName.Prompt ( PromptResult (..), prompt, continue )
import EpubTools.EpubName.Util (exitProcessingFailure)


{- Construct additional verbose output
-}
formatF :: (String, Package, Metadata) -> String
formatF (fmtUsed, _, _) = printf "\n   formatter: %s" fmtUsed

formatFM :: (String, Package, Metadata) -> String
formatFM (fmtUsed, pkg, md) =
   printf "\n   formatter: %s\n%s\n%s" fmtUsed
      (format pkg) (format md)


{- Format and display output for a book that was processed
-}
displayResults :: MonadIO m => VerbosityLevel
   -> FilePath -> FilePath -> String -> Package -> Metadata -> m ()
displayResults verbosityLevel' origPath newName fmtUsed pkg md =
   liftIO $ printf "%s -> %s%s\n" origPath newName
      (additional verbosityLevel' (fmtUsed, pkg, md))
   where
      additional Normal = const ""
      additional ShowFormatter = formatF
      additional ShowBookInfo = formatFM


{- Process an individual epub book file
-}
processBook :: Options -> [Formatter] -> [FilePath] -> Bool -> Bool
   -> IO Bool

processBook _    _          []              _     priRes =
   return priRes

processBook _    _          _               False priRes =
   return priRes

processBook opts formatters (oldPath:paths) _     priRes = do
   result <- runExceptT $ do
      {- Parse the book's metadata
         The reason for the nested runExceptT block is, if there is
         failure during parsing, we need to mark the result up with the
         file path. Failures here will otherwise get lost in the output
         when multiple books are processed at once.
      -}
      epm <- runExceptT $ do
         xml <- getPkgXmlFromZip oldPath
         (,) <$> getPackage xml <*> getMetadata xml
      (pkg, md) <- either
         ( \msg -> throwError
            $ printf "ERROR: File %s: %s" oldPath msg
         ) return epm

      (fmtUsed, newName) <-
         tryFormatting (Globals opts pkg md) formatters oldPath

      displayResults opts.verbosityLevel oldPath newName fmtUsed pkg md

      promptResult <- liftIO $ case opts.interactive.v of
        True  -> prompt
        False -> return Yes

      liftIO $ when ((promptResult == Yes) && (not opts.noAction.v)) $ do
        -- We need enough Data.List functions inside this block that it's
        -- easier to convert the NonEmpty a back into a [a]
        let destDirs = toList opts.targetDirs.v

        linkOutcomes <- sequence . map (doOneDest oldPath newName) $ destDirs

        if ((all (== True) linkOutcomes) && opts.move.v)
          then do
            putStrLn "All links successfully created, removing original file"
            removeLink oldPath
          else putStrLn "One or more links failed, NOT removing original file"

      return $ continue promptResult

   (thisRes, cont) <- either (\errmsg -> do
      putStrLn errmsg
      return (False, True)
      )
      (\c -> return (True, c))
      result

   let newRes = case (priRes, thisRes) of
         (False, _) -> False
         (_    , r) -> r

   processBook opts formatters paths cont newRes


doOneDest :: FilePath -> FilePath -> FilePath -> IO Bool
doOneDest srcPath newName destDir = do
  let destPath = destDir </> newName
  fileExists <- liftIO $ doesFileExist destPath
  if fileExists
    then do
      printf "File %s already exists. No change.\n" destPath
      pure False
    else
      tryHardLink srcPath destPath


tryHardLink :: FilePath -> FilePath -> IO Bool
tryHardLink srcFp destFp = do
  el <- try $ createLink srcFp destFp
  either tryCopy (const $ pure True) el
  where
    tryCopy :: IOException -> IO Bool
    tryCopy _ = do
      printf "Hard link at %s failed, attempting to copy instead\n" destFp
      ec <- try $ copyFile srcFp destFp
      either failureHandler (const $ pure True) ec

    failureHandler :: IOException -> IO Bool
    failureHandler _ = do
      printf "Copy to %s failed\n" destFp
      pure False


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr, stdin ]

   either exitWith exitWith =<< (runExceptT $ do
      opts <- liftIO parseOpts

      -- Locate the rules file, load it and parse into a list of formatters
      fs <- initialize opts

      when opts.noAction.v $ liftIO $ putStrLn "No-action specified"

      -- Perform the formatting operation on the books
      code <- liftIO $ processBook opts fs (toList opts.bookFiles.v) True True
      case code of
         True  -> return ExitSuccess
         False -> throwError exitProcessingFailure
      )
