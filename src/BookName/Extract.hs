{-# LANGUAGE FlexibleContexts #-}

module BookName.Extract
   ( parseFile
   , parseMeta
   )
   where

import Control.Monad.Error
import Data.Map hiding ( map )
import Data.Maybe
import HSH.Command
import Text.Regex

import BookName.Util


parseLine :: String -> Maybe (String, String)
parseLine line =
   case (fromJust $ matchRegex (mkRegex "([^:]+): (.*)") line) of
      (_:"":_)    -> Nothing
      (key:val:_) -> Just (key, val)
      [_]         -> Nothing
      []          -> Nothing


parseMeta :: String -> String -> Fields
parseMeta path raw = fromList $ catMaybes $ map parseLine allLines
   where
      allLines = ("File: " ++ path) : (lines raw)


extractMeta ::
   (MonadIO m, RunResult (IO a), MonadError [Char] m) =>
   String -> m a
extractMeta path = do
   result <- liftIO $ tryEC $ run $ "lrf-meta " ++ path
   case result of
      Left ps -> throwError $ "[ERROR lrf-meta " ++ (show ps) ++ 
         " This is probably not an LRF file.]"
      Right output -> return output


parseFile :: (MonadIO m, MonadError String m) => String -> m Fields
parseFile path = do
   output <- extractMeta path
   return $ parseMeta path output
