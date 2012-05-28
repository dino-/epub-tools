-- Copyright: 2008-2012 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module EpubTools.EpubName.Main
   ( initialize
   )
   where

import Control.Monad.Error
import Data.List ( intercalate )
import System.Directory ( doesFileExist )
import System.Exit

import EpubTools.EpubName.Format.Compile
import EpubTools.EpubName.Format.Format
import EpubTools.EpubName.Opts
import EpubTools.EpubName.Util


instance Error ExitCode where
    -- noMsg  :: a
    noMsg  = strMsg ""

    -- strMsg :: String -> a
    strMsg = const exitInitFailure


initialize :: (MonadError ExitCode m, MonadIO m) =>
   Options -> m [Formatter]
initialize opts = (locateRules opts) >>= loadFormatters


locateRules :: (MonadError ExitCode m, MonadIO m) =>
   Options -> m FilePath
locateRules opts = do
   let paths = optRulesPaths opts
   mbResult <- liftIO $ foldl (liftM2 mplus) (return Nothing)
      $ map mbExists paths
   maybe (errNoRules paths) return mbResult


mbExists :: FilePath -> IO (Maybe FilePath)
mbExists p = do
   e <- doesFileExist p
   if e
      then return $ Just p
      else return Nothing


errNoRules :: (MonadError ExitCode m, MonadIO m) =>
   [FilePath] -> m FilePath
errNoRules paths = do
   liftIO $ do
      putStrLn "Unable to find rules file. Paths searched:"
      let indentedPaths = map ((++) "   ") paths
      putStrLn $ intercalate "\n" indentedPaths
   throwError exitInitFailure


loadFormatters :: (MonadError ExitCode m, MonadIO m) =>
   FilePath -> m [Formatter]
loadFormatters rulesPath = do
   parseResult <- liftIO $ parseRules rulesPath

   case parseResult of
      Left err  -> do
         liftIO $ print err
         throwError exitInitFailure
      Right fs  -> return fs
