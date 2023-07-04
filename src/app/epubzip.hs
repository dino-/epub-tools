-- License: ISC (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Codec.Epub
import Control.Monad
import Control.Monad.Except
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO ( BufferMode ( NoBuffering )
                 , hSetBuffering, stdout, stderr 
                 )
import Text.Printf

import EpubTools.EpubName.Format.Format ( tryFormatting )
import EpubTools.EpubName.Format.Util
import EpubTools.EpubName.Main
import qualified EpubTools.EpubName.Opts as EN
import EpubTools.EpubZip.Opts


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   either exitWith exitWith =<< (runExceptT $ do
      -- Parse command-line arguments
      (opts, paths) <- liftIO $ getArgs >>= parseOpts

      -- User asked for help, this is a special termination case
      when ((optHelp opts) || (null paths)) $ do
         liftIO $ putStrLn usageText
         throwError ExitSuccess

      let inputPath = head paths
      isDir <- liftIO $ doesDirectoryExist inputPath

      en <- if isDir
            then do
               dos <- liftIO EN.defaultOptions
               fs <- initialize dos
               runExceptT $ do
                  xml <- snd `fmap` getPkgPathXmlFromDir "."
                  pkg <- getPackage xml
                  metadata <- getMetadata xml

                  (_, newPath) <- tryFormatting
                     (Globals dos pkg metadata) fs
                     "CURRENT DIRECTORY"

                  return $ inputPath </> newPath
            else return . Right $ inputPath

      case en of
         Right zipPath -> do
            exists <- liftIO $ doesFileExist zipPath

            case ((optOverwrite opts), exists) of
               (False, True) -> do
                  _ <- liftIO $ printf
                     "File %s exists, use --overwrite to force\n"
                     zipPath
                  throwError $ ExitFailure 1
               (True,  True) -> liftIO $ removeFile zipPath
               (_   ,  _   ) -> return ()

            _ <- liftIO $ do
               archive <- mkEpubArchive "."
               writeArchive zipPath archive
               printf "Book created: %s\n" zipPath

            return ExitSuccess

         Left errorMsg -> do
            liftIO $ putStrLn errorMsg
            throwError $ ExitFailure 1
      )
