-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

module EpubTools.EpubName.Main
   ( initialize
   )
   where

import Control.Monad.Except
import System.Directory ( doesFileExist )
import System.Exit
import Text.Printf

import qualified EpubTools.EpubName.Doc.Rules as Rules
import EpubTools.EpubName.Format.Compile
import EpubTools.EpubName.Format.Format
import EpubTools.EpubName.Opts
import EpubTools.EpubName.Util


initialize :: (MonadError ExitCode m, MonadIO m) =>
   Options -> m [Formatter]
initialize opts = (locateRules $ optRulesPaths opts)
   >>= (loadFormatters $ optVerbose opts)


locateRules :: (MonadError ExitCode m, MonadIO m) =>
   [FilePath] -> m (String, String)
locateRules paths = do
   mbResult <- liftIO $ foldl (liftM2 mplus) (return Nothing)
      $ map mbExists paths
   maybe (return ("built-in", Rules.defaults))
      loadRules mbResult

   where
      loadRules p = do
         contents <- liftIO $ readFile p
         return (p, contents)


mbExists :: FilePath -> IO (Maybe FilePath)
mbExists p = do
   e <- doesFileExist p
   if e
      then return $ Just p
      else return Nothing


loadFormatters :: (MonadError ExitCode m, MonadIO m) =>
   (Maybe Int) -> (String, String) -> m [Formatter]
loadFormatters verbosity (name, contents) = case parseRules name contents of
   Left err  -> do
      _ <- liftIO $ printf "%s:\n%s" name (show err)
      throwError exitInitFailure
   Right fs  -> do
      liftIO $ showRulesSource verbosity name
      return fs


showRulesSource :: (Monad m, PrintfType (m ()), PrintfArg t, Num a, Eq a) =>
   Maybe a -> t -> m ()
showRulesSource (Just 1) name = printf "Rules loaded from: %s\n" name
showRulesSource _        _    = return ()
