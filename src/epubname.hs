-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

import Codec.Epub.Opf.Format.Package
import Codec.Epub.Opf.Package
import Codec.Epub.Opf.Parse
import Control.Monad.Error
import System.Environment ( getArgs )
import System.Exit
import System.IO ( BufferMode ( NoBuffering )
                 , hSetBuffering, stdout, stderr 
                 )
import System.Posix.Files ( rename )
import Text.Printf

import EpubName.Formatters ( tryFormatting )
import EpubName.Opts ( Options (..), parseOpts, usageText )
import EpubName.Util ( runBN )


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
processBook :: Options -> ErrorT String IO (FilePath, Package)
   -> IO Bool
processBook opts parseFileAction = do
   result <- runBN $ do
      (oldPath, pkg) <- parseFileAction
      let md = opMeta pkg
      (fmtUsed, newPath) <- tryFormatting (oldPath, md)
      unless (optNoAction opts) $ liftIO $ rename oldPath newPath
      return (oldPath, newPath, fmtUsed, pkg)

   let (success, report) = either ((,) False)
         (\r -> (True, makeOutput opts r)) result
   putStrLn report
   return success


{- Thin wrapper around epub-metadata file parse
-}
parseFile :: (MonadIO m, MonadError String m)
   => FilePath -> m (String, Package)
parseFile path = do
   pkg <- parseEpubOpf path
   return (path, pkg)


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
         let parseFileActions = map parseFile paths
         codes <- mapM (processBook opts) parseFileActions
         case all id codes of
            True  -> return ExitSuccess
            False -> return . ExitFailure $ 2

   exitWith ec
