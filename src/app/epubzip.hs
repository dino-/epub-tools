import Codec.Epub (getMetadata, getPackage, getPkgPathXmlFromDir,
  mkEpubArchive, writeArchive)
import Control.Monad.Except (liftIO, runExceptT)
import System.Directory (doesDirectoryExist, doesFileExist, removeFile)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.FilePath ((</>))
import System.IO (BufferMode ( NoBuffering ), hSetBuffering, stdout, stderr)
import Text.Printf (printf)

import EpubTools.EpubName.Format.Format (tryFormatting)
import EpubTools.EpubName.Format.Util (Globals (..), throwError)
import EpubTools.EpubName.Main (initialize)
import qualified EpubTools.EpubName.Common as EN
import EpubTools.EpubZip.Opts (DirOrName (..),
  Options (dirOrName, overwrite), Overwrite (..), parseOpts)


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   either exitWith exitWith =<< (runExceptT $ do
      opts <- liftIO parseOpts

      let (DirOrName inputPath) = dirOrName opts
      isDir <- liftIO $ doesDirectoryExist inputPath

      en <- if isDir
            then do
               let dos = EN.defaultOptions
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

            case ((overwrite opts), exists) of
               (Overwrite False, True) -> do
                  _ <- liftIO $ printf
                     "File %s exists, use --overwrite to force\n"
                     zipPath
                  throwError $ ExitFailure 1
               (Overwrite True,  True) -> liftIO $ removeFile zipPath
               (_             ,  _   ) -> return ()

            _ <- liftIO $ do
               archive <- mkEpubArchive "."
               writeArchive zipPath archive
               printf "Book created: %s\n" zipPath

            return ExitSuccess

         Left errorMsg -> do
            liftIO $ putStrLn errorMsg
            throwError $ ExitFailure 1
      )
