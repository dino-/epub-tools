-- Copyright: 2008-2012 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Codec.Epub.Opf.Format.Package
import Codec.Epub.Opf.Package
import Codec.Epub.Opf.Parse
import Control.Monad
import Control.Monad.Trans
import System.Directory ( doesFileExist, renameFile )
import System.Environment ( getArgs )
import System.Exit
import System.IO ( BufferMode ( NoBuffering )
                 , hSetBuffering, stdout, stderr 
                 )
import Text.Printf

import EpubTools.EpubName.Format ( tryFormatting )
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
processBook :: Options -> FilePath -> IO Bool
processBook opts oldPath = do
   result <- runEN opts $ do
      pkg <- parseEpubOpf oldPath
      let md = opMeta pkg
      (fmtUsed, newPath) <- tryFormatting oldPath md

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

   (opts, paths) <- getArgs >>= parseOpts >>= either exitWith return

   ec <- if ((optHelp opts) || (null paths))
      then do
         putStrLn usageText
         return ExitSuccess
      else do
         when (optNoAction opts) (putStrLn "No-action specified")
         codes <- mapM (processBook opts) paths
         case all id codes of
            True  -> return ExitSuccess
            False -> return . ExitFailure $ 2

   exitWith ec
