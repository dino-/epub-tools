-- License: ISC (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

-- This is for GHC 7.8/7.10 compatibility with the
-- Control.Applicative import below
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE FlexibleContexts #-}

import Codec.Epub
import Codec.Epub.Data.Metadata
import Codec.Epub.Data.Package
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.List.NonEmpty (toList)
import System.Directory ( doesDirectoryExist, doesFileExist, renameFile )
import System.Environment ( getArgs )
import System.FilePath
import System.Exit
import System.IO ( BufferMode ( NoBuffering )
                 , hSetBuffering, stdin, stdout, stderr 
                 )
import Text.Printf

import qualified EpubTools.EpubName.Doc.Dsl as Dsl
import qualified EpubTools.EpubName.Doc.Rules as Rules
import EpubTools.EpubName.Format.Format ( Formatter (..), tryFormatting )
import EpubTools.EpubName.Format.Util
import EpubTools.EpubName.Main
import EpubTools.EpubName.Opts ( Options (..), parseOpts )
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
      (additional (optVerbose opts) (fmtUsed, pkg, md))
   where
      additional Nothing  = const ""
      additional (Just 1) = formatF
      additional _        = formatFM


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

      promptResult <- liftIO $ case (optInteractive opts) of
         True  -> prompt
         False -> return Yes

      when ((promptResult == Yes) && (optNoAction opts == False)) $
         liftIO $ renameFile oldPath newPath

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


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr, stdin ]

   either exitWith exitWith =<< (runExceptT $ do
      -- Parse command-line arguments
      -- (opts, paths) <- liftIO parseOpts
      opts <- liftIO parseOpts
      liftIO $ print opts
      void $ throwError ExitSuccess

      -- User asked for rules help, this is a special termination case
      when (optHelpRules opts) $ do
         liftIO $ putStrLn Dsl.docs
         throwError ExitSuccess

      -- User asked for a dump of the built-in rules,
      -- this is a special termination case
      when (optDumpRules opts) $ do
         liftIO $ putStr Rules.defaults
         throwError ExitSuccess

      -- FIXME Should we even be doing this here?
      -- -- User asked for help, this is a special termination case
      -- when ((optHelp opts) || (null paths)) $ do
      --    liftIO $ usageText >>= putStrLn
      --    throwError ExitSuccess

      let targetDir = optTargetDir opts
      targetDirExists <- liftIO $ doesDirectoryExist targetDir
      unless targetDirExists $ do
         _ <- liftIO $ printf
            "ERROR: Target directory doesn't exist: %s\n" targetDir
         throwError exitInitFailure

      -- Locate the rules file, load it and parse into a list of
      -- formatters
      fs <- initialize opts

      when (optNoAction opts) $ liftIO
         $ putStrLn "No-action specified"

      -- Perform the formatting operation on the books
      code <- liftIO $ processBook opts fs (toList . optFiles $ opts) True True
      case code of
         True  -> return ExitSuccess
         False -> throwError exitProcessingFailure
      )
