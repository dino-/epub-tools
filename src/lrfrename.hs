#! /usr/bin/env runhaskell

{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Error
import Data.Either
import Data.Map hiding ( map )
import Data.Maybe
import HSH.Command
import Prelude hiding ( lookup )
import System.Environment
import Text.Regex


allKeys :: [String]
allKeys =
   [ "File"
   , "Author"
   , "Author.reading"
   , "BookID"
   , "Category"
   , "Classification"
   , "CreationDate"
   , "Creator"
   , "FreeText"
   , "Label"
   , "Language"
   , "SumPage"
   , "Producer"
   , "Publisher"
   , "Title"
   , "Title.reading"
   ]


parseLine :: String -> Maybe (String, String)
parseLine line =
   case (fromJust $ matchRegex (mkRegex "(.*): (.*)") line) of
      [_  , "" ] -> Nothing
      [key, val] -> Just (key, val)


parseMeta :: String -> String -> Map String String
parseMeta path raw = fromList $ catMaybes $ map parseLine allLines
   where
      allLines = ("File: " ++ path) : (lines raw)


extractMeta ::
   (MonadIO m, RunResult (IO a), MonadError [Char] m) =>
   String -> m a
extractMeta path = do
   result <- liftIO $ tryEC $ run $ "lrf-meta " ++ path ++ " 2>/dev/null"
   case result of
      Left ps -> throwError $ 
         "ERROR: File " ++ path ++ " is probably not an LRF file"
      Right output -> return output


parseFile :: (MonadIO m) => 
   String -> m (Either String (Map String String))
parseFile path = runErrorT $ do
   output <- extractMeta path
   return $ parseMeta path output


catRights = foldr f []
   where
      f e es = case e of
         Right x -> x : es
         _       -> es


displayAuthorTitle fs = putStrLn $ author ++ " | " ++ title
   where
      author = fromJust $ lookup "Author" fs
      title = fromJust $ lookup "Title" fs


main :: IO ()
main = do
   paths <- getArgs

   results <- mapM parseFile paths
   --mapM_ (either putStrLn ((mapM_ print) . toList)) results
   let good = catRights results
   mapM_ displayAuthorTitle good
