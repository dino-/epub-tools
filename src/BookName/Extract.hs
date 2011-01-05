{-# LANGUAGE FlexibleContexts #-}

module BookName.Extract
   ( parseFile
--   , parseMeta
   )
   where

import Codec.Epub.Opf.Package
import Codec.Epub.Opf.Parse
import Control.Monad.Error
--import Data.Map hiding ( map )
--import Data.Maybe
--import HSH.Command
--import System.FilePath
--import Text.Printf
--import Text.Regex

--import BookName.Util


{-
parseLine :: String -> Maybe (String, String)
parseLine line =
   case (fromJust $ matchRegex (mkRegex "([^:]+): (.*)") line) of
      (_:"":_)    -> Nothing
      (key:val:_) -> Just (transformKey key, val)
      [_]         -> Nothing
      []          -> Nothing


trimSpaces :: String -> String
trimSpaces = reverse . (dropWhile (== ' ')) . reverse


transformKey :: String -> String
transformKey "Author(s)           " = "Authors"
transformKey key                    = trimSpaces key


trimBracketed :: String -> String
trimBracketed s
   | '[' `elem` s = init $ takeWhile (/= '[') s
   | otherwise  = s


parseMeta :: String -> String -> Fields
parseMeta path raw = adjust trimBracketed "Authors" . fromList
   . catMaybes . map parseLine $ allLines
   where
      allLines = ("File: " ++ path) : (lines raw)


extractMeta ::
   (MonadIO m, RunResult (IO a), MonadError [Char] m) =>
   String -> m a
extractMeta path = do
   let app = "ebook-meta"
   result <- liftIO $ tryEC $ run ((printf "%s %s" app path) :: String)
   case result of
      Left ps -> throwError $
         printf "[ERROR %s  path: %s  status: %s]" app path (show ps)
      Right output -> return output
-}


parseFile :: (MonadIO m, MonadError String m)
   => FilePath -> m (String, Metadata)
parseFile path = do
   pkg <- parseEpubOpf path
   return (path, opMeta pkg)
