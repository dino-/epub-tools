#! /usr/bin/env stack
{- stack runghc -}

{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import Control.Monad
import Data.List
import Data.Version
import Distribution.Package
import Distribution.PackageDescription hiding ( error, options )
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Distribution.Version
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process
import Text.Printf
import Text.Read


defaultOptions :: Options
defaultOptions = Options
   { optClean = False
   , optDelete = False
   , optHelp = False
   , optLink = False
   , optPrefix = "/opt"
   , optRsrcCpVerbose = True
   , optInstType = FHS
   , optVersion = True
   }

data InstallType = Bundle | FHS deriving Eq


main :: IO ()
main = do
   -- Parse args
   (opts, _) <- parseOpts =<< getArgs

   -- User asked for help
   when (optHelp opts) $ putStrLn usageText >> exitSuccess

   -- Locate cabal file
   cabalFiles <- (filter $ isSuffixOf ".cabal") <$> getDirectoryContents "."

   when (null cabalFiles) $ do
      die "Can't continue because no cabal files were found in ."

   -- Parse the cabal file and extract things we need from it
   -- then pass a pile of what we know to a function to create the
   -- installation dirs
   dirs <- constructDirs opts . package . packageDescription
      <$> readPackageDescription normal (head cabalFiles)


   -- Perform the installation

   -- Remove existing install directory
   appDirExists <- doesDirectoryExist $ appDir dirs
   when (optDelete opts && appDirExists) $ do
      putStrLn $ "Removing existing directory " ++ (appDir dirs)
      removeDirectoryRecursive $ appDir dirs

   -- Clean before building
   when (optClean opts) $ system "stack clean" >> return ()

   -- Copy the binaries
   createDirectoryIfMissing True $ binDir dirs
   installExitCode <- system $ "stack install --local-bin-path=" ++ (binDir dirs)
   unless (ok installExitCode) $ die "Can't continue because stack install failed"

   -- Copy additional scripts
   {-
   putStrLn "Copying additional scripts"
   mapM_ (\f -> copyFile ("util" </> f) (binDir dirs </> f))
      [ "script1.sh", "script2.hs" ]
   -}

   -- Copy the license
   putStrLn "\nCopying LICENSE"
   createDirectoryIfMissing True $ docDir dirs
   copyFile "LICENSE" (docDir dirs </> "LICENSE")

   -- Copy the resources
   let rsrcDirSrc = "." </> "resources"
   rsrcsExist <- doesDirectoryExist rsrcDirSrc
   when rsrcsExist $ do
      putStrLn $ "\nCopying resources"
      copyTree (optRsrcCpVerbose opts) rsrcDirSrc (rsrcDir dirs)
      return ()

   -- Make the symlink
   when (optLink opts) $ do
      if (optInstType opts == FHS) then
         putStrLn "No link will be made because installation type is fhs"
      else if (not . optVersion $ opts) then
         putStrLn "No link will be made because the app dir already has no version part"
      else do
         printf "Making symbolic link now %s -> %s\n" (linkPath dirs) (appDir dirs)
         system $ printf "rm %s" (linkPath dirs)
         system $ printf "ln -s %s %s" (appDir dirs) (linkPath dirs)
         return ()

   exitSuccess


data Dirs = Dirs
   { appDir :: FilePath
   , linkPath :: FilePath
   , binDir :: FilePath
   , docDir :: FilePath
   , rsrcDir :: FilePath
   }


constructDirs :: Options -> PackageId -> Dirs
constructDirs opts pkgId =
   Dirs appDir' linkPath' binDir' (appDir' </> "doc") (appDir' </> "resources")

   where
      project = unPackageName . pkgName $ pkgId
      version = showVersion . pkgVersion $ pkgId
      versionPart = if optVersion opts then "-" ++ version else ""
      appDir' = case (optInstType opts) of
         Bundle -> optPrefix opts </> (project ++ versionPart)
         FHS    -> optPrefix opts </> "share" </> (project ++ versionPart)
      linkPath' = optPrefix opts </> project
      binDir' = case (optInstType opts) of
         Bundle -> appDir' </> "bin"
         FHS    -> optPrefix opts </> "bin"


{- Turn an exit code (say, from system) into a Bool
-}
ok :: ExitCode -> Bool
ok ExitSuccess = True
ok _           = False


{-
   Argument parsing code
-}

data Options = Options
   { optClean :: Bool
   , optDelete :: Bool
   , optHelp :: Bool
   , optLink :: Bool
   , optPrefix :: FilePath
   , optRsrcCpVerbose :: Bool
   , optInstType :: InstallType
   , optVersion :: Bool
   }


instance Read InstallType where
   readsPrec _ "bundle" = [(Bundle, "")]
   readsPrec _ "fhs"    = [(FHS, "")]
   readsPrec _ _        = []

instance Show InstallType where
   show Bundle = "bundle"
   show FHS = "fhs"


readInstallType :: String -> InstallType
readInstallType s =
   case (readEither s) of
      Left _ -> error $ printf "Can't continue because %s is not a valid install type\n\n%s" s usageText
      Right t -> t


options :: [OptDescr (Options -> Options)]
options =
   [ Option ['c'] ["clean"]
      (NoArg (\opts -> opts { optClean = True } ))
      ("Do 'stack clean' first." ++ (defaultText . optClean $ defaultOptions))
   , Option ['C'] ["no-clean"]
      (NoArg (\opts -> opts { optClean = False } ))
      ("Do not 'stack clean' first."
         ++ (defaultText . not . optClean $ defaultOptions))
   , Option ['d'] ["delete"]
      (NoArg (\opts -> opts { optDelete = True } ))
      ("Delete the app directory before copying files."
         ++ (defaultText . optDelete $ defaultOptions))
   , Option ['D'] ["no-delete"]
      (NoArg (\opts -> opts { optDelete = False } ))
      ("Do not delete the app directory before copying files."
         ++ (defaultText . not . optDelete $ defaultOptions))
   , Option ['h'] ["help"]
      (NoArg (\opts -> opts { optHelp = True } ))
      "This help information."
   , Option ['l'] ["link"]
      (NoArg (\opts -> opts { optLink = True } ))
      ("Create symlink PROJECT -> PROJECT-VERSION in PREFIX dir. Only useful for bundle installations. Does not work on Windows."
         ++ (defaultText . optLink $ defaultOptions))
   , Option ['L'] ["no-link"]
      (NoArg (\opts -> opts { optLink = True } ))
      ("Do not create symlink PROJECT -> PROJECT-VERSION in PREFIX dir."
         ++ (defaultText . not . optLink $ defaultOptions))
   , Option ['p'] ["prefix"]
      (ReqArg (\s opts -> opts { optPrefix = s } ) "PREFIX" )
      (printf "Install prefix directory. Defaults to %s so what you'll end up with is %s/PROJECT-VERSION"
         (optPrefix defaultOptions) (optPrefix defaultOptions))
   , Option ['r'] ["resource-copy-verbose"]
      (NoArg (\opts -> opts { optRsrcCpVerbose = True } ))
      ("Be chatty when copying the resources directory."
         ++ (defaultText . optRsrcCpVerbose $ defaultOptions))
   , Option ['R'] ["no-resource-copy-verbose"]
      (NoArg (\opts -> opts { optRsrcCpVerbose = False } ))
      ("Don't be chatty when copying the resources directory. Useful when there are a LOT of resources."
         ++ (defaultText . not . optRsrcCpVerbose $ defaultOptions))
   , Option ['t'] ["type"]
      (ReqArg (\s opts -> opts { optInstType = readInstallType s } ) "INST_TYPE" )
      (printf "Installation type, see INSTALLATION TYPE below for details. Default: %s"
         (show . optInstType $ defaultOptions))
   , Option ['v'] ["version"]
      (NoArg (\opts -> opts { optVersion = True } ))
      (printf "Include version in installation path, meaning: %s/PROJECT-VERSION %s"
         (optPrefix defaultOptions) (defaultText . optVersion $ defaultOptions))
   , Option ['V'] ["no-version"]
      (NoArg (\opts -> opts { optVersion = False } ))
      (printf "Do not include version in installation path, meaning: %s/PROJECT %s"
         (optPrefix defaultOptions) (defaultText . not . optVersion $ defaultOptions))
   ]


defaultText :: Bool -> String
defaultText True  = " Default"
defaultText False = ""


parseOpts :: [String] -> IO (Options, [String])
parseOpts args =
   case getOpt Permute options args of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError $ userError (concat errs ++ usageText)


usageText :: String
usageText = (usageInfo header options) ++ "\n" ++ footer
   where
      header = init $ unlines
         [ "Usage: install.hs [OPTIONS]"
         , ""
         , "options:"
         ]
      footer = init $ unlines
         [ "INSTALLATION TYPE"
         , ""
         , "This is the topology used when copying files, one of: bundle, fhs"
         , ""
         , "bundle is sort-of a self-contained structure like this:"
         , ""
         , "  $PREFIX/"
         , "    $PROJECT -> $PROJECT-$VERSION    <-- if --link was specified"
         , "    $PROJECT-$VERSION/    <-- this is the \"app directory\""
         , "      bin/..."
         , "      doc/LICENSE"
         , "      resources/..."
         , ""
         , "fhs is the more traditional UNIX structure like this:"
         , ""
         , "  $PREFIX/"
         , "    bin/..."
         , "    share/"
         , "      $PROJECT-$VERSION/  <-- this is the \"app directory\""
         , "        doc/LICENSE"
         , "        resources/..."
         , ""
         , "Be aware that when the --delete switch is used along with fhs type, the binaries WILL NOT be deleted, only the \"app directory\"."
         , ""
         , "COMPILING"
         , ""
         , "install.hs was intentionally left as a script, but if you would prefer to compile it, do this:"
         , ""
         , "  $ stack ghc -- -o util/install util/install.hs"
         , ""
         , ""
         , "This script is part of the hsinstall package by Dino Morelli <dino@ui3.info>"
         ]


{-
   Recursive file copying code

   It was desireable to have a standalone recursive file copy in
   this script for maximum cross-platform compatibility and to
   avoid Haskell library dependencies.

   Many thanks to [abuzittin gillifirca](https://codereview.stackexchange.com/users/20251/abuzittin-gillifirca) for the StackOverflow post [Copying files in Haskell](https://codereview.stackexchange.com/questions/68908/copying-files-in-haskell) where the following code was lifted.
-}

copyTree :: Bool -> FilePath -> FilePath -> IO ()
copyTree chatty s t = do
    createDirectoryIfMissing True t
    subItems <- getSubitems s
    mapM_ (copyItem chatty s t) subItems


getSubitems :: FilePath -> IO [(Bool, FilePath)]
getSubitems path = getSubitems' ""
  where
    getChildren path =  (\\ [".", ".."]) <$> getDirectoryContents path

    getSubitems' relPath = do
        let absPath = path </> relPath
        isDir <- doesDirectoryExist absPath
        children <- if isDir then getChildren absPath else return []
        let relChildren = [relPath </> p | p <- children]
        ((isDir, relPath) :) . concat <$> mapM getSubitems' relChildren


copyItem :: Bool -> FilePath -> FilePath -> (Bool, FilePath) -> IO ()
copyItem chatty baseSourcePath baseTargetPath (isDir, relativePath) = do
    let sourcePath = baseSourcePath </> relativePath
    let targetPath = baseTargetPath </> relativePath

    when chatty $
       putStrLn $ "Copying " ++ sourcePath ++ " to " ++ targetPath

    if isDir
      then createDirectoryIfMissing False targetPath
      else copyFile sourcePath targetPath
