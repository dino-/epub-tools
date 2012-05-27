-- Copyright: 2008-2012 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

import Codec.Epub.Opf.Format.Package
import Codec.Epub.Opf.Package
import Codec.Epub.Opf.Parse
import Control.Monad
import Control.Monad.Error
import System.Directory ( doesFileExist, renameFile )
import System.Environment ( getArgs )
import System.Exit
import System.IO ( BufferMode ( NoBuffering )
                 , hSetBuffering, stdout, stderr 
                 )
import Text.Printf

import EpubTools.EpubName.Format.Format
import EpubTools.EpubName.Main
import EpubTools.EpubName.Opts ( Options (..), parseOpts, usageText )
import EpubTools.EpubName.Util


{- Construct additional verbose output
-}
formatF :: (String, Package) -> String
formatF (fmtUsed, _) = printf "\n   formatter: %s" fmtUsed

formatFM :: (String, Package) -> String
formatFM (fmtUsed, pkg) =
   printf "\n   formatter: %s\n%s" fmtUsed
      (formatPackage False pkg)


{- Format output for a book that was processed
-}
makeOutput :: Options -> (FilePath, FilePath, String, Package) -> String
makeOutput opts (oldPath, newPath, fmtUsed, pkg) =
   printf "%s -> %s%s" oldPath newPath
      (additional (optVerbose opts) (fmtUsed, pkg))
   where
      additional Nothing  = const ""
      additional (Just 1) = formatF
      additional _        = formatFM


{- Process an individual epub book file
-}
processBook :: Options -> [Formatter] -> FilePath -> IO Bool
processBook opts formatters oldPath = do
   result <- runErrorT $ do
      {- If there is failure during parsing, we need to mark the result
         up with the file path. Failures here will otherwise get lost
         in the output when multiple books are processed at once.
      -}
      epkg <- runErrorT $ parseEpubOpf oldPath
      pkg <- either
         ( \msg -> throwError
            $ printf "ERROR: File %s: %s" oldPath msg
         ) return epkg

      (fmtUsed, newPath) <-
         tryFormatting opts formatters (opMeta pkg) oldPath

      when (not $ optOverwrite opts) $ do
         fileExists <- liftIO $ doesFileExist newPath
         when fileExists $ throwError $ 
            printf "ERROR: File %s already exists!" newPath

      unless (optNoAction opts) $ liftIO $ renameFile oldPath newPath

      return (oldPath, newPath, fmtUsed, pkg)

   let (success, report) = either ((,) False)
         (\r -> (True, makeOutput opts r)) result
   putStrLn report
   return success


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   either exitWith exitWith =<< (runErrorT $ do
      -- Parse command-line arguments
      (opts, paths) <- (liftIO getArgs) >>= parseOpts

      -- User asked for help, this is a special termination case
      when ((optHelp opts) || (null paths)) $ do
         liftIO $ putStrLn usageText
         throwError ExitSuccess

      when (optNoAction opts) $ liftIO
         $ putStrLn "No-action specified"

      -- Locate the rules file, load it and parse into a list of
      -- formatters
      fs <- initialize opts

      -- Perform the formatting operation on the books
      codes <- liftIO $ mapM (processBook opts fs) paths
      case all id codes of
         True  -> return ExitSuccess
         False -> throwError exitProcessingFailure
      )
