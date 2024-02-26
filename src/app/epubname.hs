-- License: ISC (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

-- This is for GHC 7.8/7.10 compatibility with the
-- Control.Applicative import below
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE FlexibleContexts, OverloadedRecordDot #-}

import Codec.Epub
import Codec.Epub.Data.Metadata
import Codec.Epub.Data.Package
import Control.Applicative
import Control.Exception (try)
import Control.Monad
import Control.Monad.Except
import Data.List.NonEmpty (toList)
import GHC.IO.Exception (IOException)
import System.Directory (copyFile, doesDirectoryExist, doesFileExist)
import System.FilePath
import System.Exit
import System.IO ( BufferMode ( NoBuffering )
                 , hSetBuffering, stdin, stdout, stderr 
                 )
import System.Posix (createLink, removeLink)
import Text.Printf

import EpubTools.EpubName.Common
  ( BookFiles (..)
  , DumpRulesSwitch (..)
  , HelpRulesSwitch (..)
  , InteractiveSwitch (..)
  , MoveSwitch (..)
  , NoActionSwitch (..)
  , Options (bookFiles, dumpRules, helpRules, interactive, move, noAction,
      optTargetDir, verbosityLevel)
  , VerbosityLevel (Normal, ShowFormatter, ShowBookInfo)
  )
import qualified EpubTools.EpubName.Doc.Dsl as Dsl
import qualified EpubTools.EpubName.Doc.Rules as Rules
import EpubTools.EpubName.Format.Format ( Formatter (..), tryFormatting )
import EpubTools.EpubName.Format.Util
import EpubTools.EpubName.Main
import EpubTools.EpubName.Opts (parseOpts)
import EpubTools.EpubName.Prompt ( PromptResult (..), prompt, continue )
import EpubTools.EpubName.Util


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
displayResults :: MonadIO m => Options
   -> FilePath -> FilePath -> String -> Package -> Metadata -> m ()
displayResults opts oldPath newPath fmtUsed pkg md =
   liftIO $ printf "%s -> %s%s\n" oldPath newPath
      (additional (verbosityLevel opts) (fmtUsed, pkg, md))
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
         The reason for the nested runErrorT block is, if there is
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

      (fmtUsed, shortPath) <-
         tryFormatting (Globals opts pkg md) formatters oldPath

      let newPath = optTargetDir opts </> shortPath

      fileExists <- liftIO $ doesFileExist newPath
      when fileExists $ throwError $ 
         printf "File %s already exists. No change." newPath

      displayResults opts oldPath newPath fmtUsed pkg md

      promptResult <- liftIO $ case opts.interactive.v of
         True  -> prompt
         False -> return Yes

      liftIO $ when ((promptResult == Yes) && (opts.noAction.v == False)) $ do
        succeeded <- tryHardLink oldPath newPath
        when (succeeded && opts.move.v) $ removeLink oldPath

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


tryHardLink :: FilePath -> FilePath -> IO Bool
tryHardLink srcFp destFp = do
  el <- try $ createLink srcFp destFp
  either tryCopy (const $ pure True) el
  where
    tryCopy :: IOException -> IO Bool
    tryCopy _ = do
      putStrLn "Hard link failed, attempting to copy instead"
      ec <- try $ copyFile srcFp destFp
      either failureHandler (const $ pure True) ec

    failureHandler :: IOException -> IO Bool
    failureHandler _ = do
      putStrLn "Copy failed"
      pure False


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr, stdin ]

   either exitWith exitWith =<< (runExceptT $ do
      opts <- liftIO parseOpts
      -- FIXME For debugging
      -- liftIO $ print opts
      -- void $ throwError ExitSuccess

      -- User asked for rules help, this is a special termination case
      when opts.helpRules.v $ do
         liftIO $ putStrLn Dsl.docs
         throwError ExitSuccess

      -- User asked for a dump of the built-in rules,
      -- this is a special termination case
      when opts.dumpRules.v $ do
         liftIO $ putStr Rules.defaults
         throwError ExitSuccess

      let targetDir = optTargetDir opts
      targetDirExists <- liftIO $ doesDirectoryExist targetDir
      unless targetDirExists $ do
         void . liftIO $ printf
            "ERROR: Target directory doesn't exist: %s\n" targetDir
         throwError exitInitFailure

      -- Locate the rules file, load it and parse into a list of
      -- formatters
      fs <- initialize opts

      when opts.noAction.v $ liftIO
         $ putStrLn "No-action specified"

      -- Perform the formatting operation on the books
      code <- liftIO $ processBook opts fs (toList opts.bookFiles.v) True True
      case code of
         True  -> return ExitSuccess
         False -> throwError exitProcessingFailure
      )
