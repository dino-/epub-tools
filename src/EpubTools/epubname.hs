-- Copyright: 2008-2012 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

import Codec.Epub.Opf.Format.Package
import Codec.Epub.Opf.Package
import Codec.Epub.Opf.Parse
import Control.Monad
import Control.Monad.Error
import System.Directory ( doesDirectoryExist, doesFileExist, renameFile )
import System.Environment ( getArgs )
import System.FilePath
import System.Exit
import System.IO ( BufferMode ( NoBuffering )
                 , hSetBuffering, stdin, stdout, stderr 
                 )
import Text.Printf

import EpubTools.EpubName.Format.Format ( Formatter (..), tryFormatting )
import EpubTools.EpubName.Main
import EpubTools.EpubName.Opts ( Options (..), parseOpts, usageText )
import EpubTools.EpubName.Prompt ( PromptResult (..), prompt, continue )
import EpubTools.EpubName.Util
import Paths_epub_tools


{- Construct additional verbose output
-}
formatF :: (String, Package) -> String
formatF (fmtUsed, _) = printf "\n   formatter: %s" fmtUsed

formatFM :: (String, Package) -> String
formatFM (fmtUsed, pkg) =
   printf "\n   formatter: %s\n%s" fmtUsed
      (formatPackage False pkg)


{- Format and display output for a book that was processed
-}
displayResults :: (MonadError String m, MonadIO m) =>
   Options -> FilePath -> FilePath -> String -> Package -> m ()
displayResults opts oldPath newPath fmtUsed pkg =
   liftIO $ printf "%s -> %s%s\n" oldPath newPath
      (additional (optVerbose opts) (fmtUsed, pkg))
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
   result <- runErrorT $ do
      {- Parse the book's metadata
         The reason for the nested runErrorT block is, if there is
         failure during parsing, we need to mark the result up with the
         file path. Failures here will otherwise get lost in the output
         when multiple books are processed at once.
      -}
      epkg <- runErrorT $ parseEpubOpf oldPath
      pkg <- either
         ( \msg -> throwError
            $ printf "ERROR: File %s: %s" oldPath msg
         ) return epkg

      (fmtUsed, shortPath) <-
         tryFormatting opts formatters (opMeta pkg) oldPath

      let newPath = optTargetDir opts </> shortPath

      fileExists <- liftIO $ doesFileExist newPath
      when fileExists $ throwError $ 
         printf "File %s already exists. No change." newPath

      displayResults opts oldPath newPath fmtUsed pkg

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

   either exitWith exitWith =<< (runErrorT $ do
      -- Parse command-line arguments
      (opts, paths) <- (liftIO getArgs) >>= parseOpts

      -- User asked for rules help, this is a special termination case
      when (optHelpRules opts) $ do
         liftIO $ do
            rulesHelpPath <- getDataFileName "epubname-dsl-docs"
            readFile rulesHelpPath >>= putStrLn
         throwError ExitSuccess

      -- User asked for help, this is a special termination case
      when ((optHelp opts) || (null paths)) $ do
         liftIO $ usageText >>= putStrLn
         throwError ExitSuccess

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
      code <- liftIO $ processBook opts fs paths True True
      case code of
         True  -> return ExitSuccess
         False -> throwError exitProcessingFailure
      )
