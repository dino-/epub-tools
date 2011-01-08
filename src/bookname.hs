{-# LANGUAGE FlexibleContexts #-}

import Codec.Epub.Opf.Format.Package
import Codec.Epub.Opf.Package
import Codec.Epub.Opf.Parse
import Control.Monad.Error
import System.Environment ( getArgs )
import System.IO ( BufferMode ( NoBuffering )
                 , hSetBuffering, stdout, stderr 
                 )
import System.Posix.Files ( rename )
import Text.Printf

import BookName.Formatters ( tryFormatting )
import BookName.Opts ( Options (..), parseOpts, usageText )
import BookName.Util ( runBN )


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
processBook :: Options -> ErrorT String IO (FilePath, Package) -> IO ()
processBook opts parseFileAction = do
   result <- runBN $ do
      (oldPath, pkg) <- parseFileAction
      let md = opMeta pkg
      (fmtUsed, newPath) <- tryFormatting (oldPath, md)
      unless (optNoAction opts) $ liftIO $ rename oldPath newPath
      return (oldPath, newPath, fmtUsed, pkg)

   let report = either id (makeOutput opts) result
   putStrLn report


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

   (opts, paths) <- getArgs >>= parseOpts

   if ((optHelp opts) || (null paths))
      then putStrLn usageText
      else do
         when (optNoAction opts) (putStrLn "No-action specified")
         let parseFileActions = map parseFile paths
         mapM_ (processBook opts) parseFileActions
