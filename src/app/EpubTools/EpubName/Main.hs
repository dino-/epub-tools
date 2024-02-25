-- License: ISC (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts, TupleSections #-}

module EpubTools.EpubName.Main
   ( initialize
   )
   where

import Control.Monad.Except
import Data.List.NonEmpty hiding (map)
import Data.Maybe (fromMaybe)
import Data.Monoid
import System.Directory ( doesFileExist )
import System.Environment
import System.Exit
import System.FilePath
import System.IO.Error (tryIOError)
import Text.Printf

import qualified EpubTools.EpubName.Doc.Rules as Rules
import EpubTools.EpubName.Format.Compile
import EpubTools.EpubName.Format.Format
import EpubTools.EpubName.Opts (Options (..),
  RulesLocation (BuiltinRules, RulesPath, RulesViaEnv), RulesLocations (..))
import EpubTools.EpubName.Util


initialize :: (MonadError ExitCode m, MonadIO m) =>
   Options -> m [Formatter]
initialize opts = do
  erls <- liftIO . tryIOError . locateRules . optRulesPaths $ opts
  rls <- case erls of
    Right rls' -> pure rls'
    Left err   -> do
      _ <- liftIO $ print err
      throwError exitInitFailure
  parseFormatters (optVerbose opts) rls


locateRules :: RulesLocations -> IO (String, String)
locateRules (RulesLocations rls) = do
  let loadActions = map loadRuleFile . toList $ rls
  mbLoadedFiles <- sequence loadActions
  pure $ fromMaybe undefined . getFirst . mconcat . map First $ mbLoadedFiles


loadRuleFile :: RulesLocation -> IO (Maybe (String, String))

loadRuleFile (RulesPath filePath) = do
  mbExistFile <- mbExists filePath
  case mbExistFile of
    Nothing -> pure Nothing
    (Just path) -> (Just . (filePath, )) <$> readFile path

loadRuleFile (RulesViaEnv varName pathSuffix) = do
  mbPathPrefix <- lookupEnv varName
  let mbFullPath = (</> pathSuffix) <$> mbPathPrefix
  mbExistFile <- maybe (pure Nothing) mbExists mbFullPath
  case mbExistFile of
    Nothing -> pure Nothing
    (Just path) -> (Just . (path,)) <$> readFile path

loadRuleFile BuiltinRules = pure $ Just ("built-in", Rules.defaults)


mbExists :: FilePath -> IO (Maybe FilePath)
mbExists p = do
   e <- doesFileExist p
   if e
      then return $ Just p
      else return Nothing


parseFormatters :: (MonadError ExitCode m, MonadIO m) =>
   (Maybe Int) -> (String, String) -> m [Formatter]
parseFormatters verbosity (name, contents) = case parseRules name contents of
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
