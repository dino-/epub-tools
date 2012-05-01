-- Copyright: 2011, 2012 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Codec.Epub.Archive
import Codec.Epub.IO
import Codec.Epub.Opf.Package
import Codec.Epub.Opf.Parse
import Control.Monad
import Control.Monad.Error
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import Text.Printf

import EpubTools.EpubName.Format ( tryFormatting )
import qualified EpubTools.EpubName.Opts as EN
import EpubTools.EpubName.Util ( Globals (..), runEN )
import EpubTools.EpubZip.Opts


exitFail :: String -> IO ()
exitFail msg = do
      putStrLn msg
      exitWith $ ExitFailure 1


main :: IO ()
main = do
   (opts, paths) <- getArgs >>= parseOpts

   when ((optHelp opts) || (null paths)) $ exitFail usageText

   let inputPath = head paths
   isDir <- doesDirectoryExist inputPath
   en <- if isDir
         then runErrorT $ do
            package <- do
               (_, contents) <- opfContentsFromDir "."
               parseXmlToOpf contents

            let efmt = runEN (Globals EN.defaultOptions
                  (opMeta package)) $ tryFormatting "CURRENT DIRECTORY"
            (_, newPath) <- either throwError return efmt

            return $ inputPath </> newPath
         else return . Right $ inputPath

   case en of
      Right zipPath -> do
         exists <- doesFileExist zipPath
         when ((not . optOverwrite $ opts) && exists) $ do
            exitFail $
               printf "File %s exists, use --overwrite to force" zipPath

         archive <- mkEpubArchive "."
         writeArchive zipPath archive

         printf "Book created: %s\n" zipPath

      Left errorMsg -> exitFail errorMsg
