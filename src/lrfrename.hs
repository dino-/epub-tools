#! /usr/bin/env runhaskell

{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Error
import Data.Char
import Data.Either
import Data.List hiding ( lookup )
import Data.Map hiding ( map )
import Data.Maybe
import HSH.Command
import Prelude hiding ( lookup )
import System.Environment
import System.FilePath
import Text.Printf
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
   result <- liftIO $ tryEC $ run $ "lrf-meta " ++ path
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


display1 fs = putStrLn $ author ++ " | " ++ title
   where
      author = l "Author" fs
      title = l "Title" fs


{-
display2 fs = printf "%s | %s | %s | %s-%s\n"
   (takeFileName $ l "File" fs)
   author
   title
   (fromMaybe "[FAILED]" $ newAuthor author)
   (newTitle title)
   where
      author = l "Author" fs
      title = l "Title" fs
-}


display3 (Left msg) = putStrLn msg

display3 (Right fs) = printf "%s | %s | %s | %s%s\n"
   (takeFileName $ l "File" fs)
   author
   title
   (format authorPatterns author)
   (format titlePatterns title)
   where
      author = l "Author" fs
      title = l "Title" fs


display4 (Left msg) = putStrLn msg

display4 (Right fs) = printf "%s%s | %s | %s | %s\n"
   (format authorPatterns author)
   (format titlePatterns title)
   author
   title
   (takeFileName $ l "File" fs)
   where
      author = l "Author" fs
      title = l "Title" fs


l k m = fromMaybe ("[ERROR no key: " ++ k ++ "]") $ lookup k m


nameFilters :: [(String -> String)]
nameFilters =
   [ (\s -> subRegex (mkRegex "[.',\\?();#]") s "")
   , (\s -> subRegex (mkRegex "]") s "")
   , (\s -> subRegex (mkRegex "\\*") s "")
   , (\s -> subRegex (mkRegex "!") s "")
   , (\s -> subRegex (mkRegex "-") s " ")
   , (\s -> subRegex (mkRegex "\\[") s "_")
   , (\s -> subRegex (mkRegex "^The ") s "")
   , (\s -> subRegex (mkRegex "&") s " And ")
   , capFirstAndDeSpace
   ]


capFirstAndDeSpace s = concat $ map capFirst $ words s
   where capFirst (first:rest) = (toUpper first) : rest


authorDouble (_:last1:_:last2:_) = last1' ++ "_" ++ last2' ++ "-"
   where
      last1' = foldl (flip id) last1 nameFilters
      last2' = foldl (flip id) last2 nameFilters


authorSingle (rest:last:_) = last' ++ rest' ++ "-"
   where
      last' = foldl (flip id) last nameFilters
      rest' = foldl (flip id) rest nameFilters


titleSimple (old:_) = foldl (flip id) old nameFilters


titleAeonMag (numWord:_) = "AeonMagazine" ++ (num numWord)
   where
      num "One"       = "01"
      num "Two"       = "02"
      num "Three"     = "03"
      num "Four"      = "04"
      num "Five"      = "05"
      num "Six"       = "06"
      num "Seven"     = "07"
      num "Eight"     = "08"
      num "Nine"      = "09"
      num "Ten"       = "10"
      num "Eleven"    = "11"
      num "Twelve"    = "12"
      num "Thirteen"  = "13"
      num "Fourteen"  = "14"
      num "Fifteen"   = "15"
      num "Sixteen"   = "16"
      num "Seventeen" = "17"
      num "Eighteen"  = "18"
      num "Nineteen"  = "19"
      num "Twenty"    = "20"


titleFsfMag (prefix:rest) =
   titleMagYM ((prefix ++ "Magazine"):rest)


titleMagYM (prefix:month:year:_) =
   prefix' ++ year ++ "-" ++ (monthNum month)
   where
      prefix' = foldl (flip id) prefix nameFilters
      monthNum "January"            = "01"
      monthNum "January-February"   = "01_02"
      monthNum "February"           = "02"
      monthNum "March"              = "03"
      monthNum "April"              = "04"
      monthNum "April-May"          = "04_05"
      monthNum "May"                = "05"
      monthNum "June"               = "06"
      monthNum "July"               = "07"
      monthNum "July-August"        = "07_08"
      monthNum "Jul-Aug"            = "07_08"
      monthNum "August"             = "08"
      monthNum x
         | isPrefixOf x "September" = "09"
      monthNum "October"            = "10"
      monthNum "October-November"   = "10_11"
      monthNum "November"           = "11"
      monthNum "December"           = "12"
      monthNum x                    = x


titleMagInterzone (prefix:num:_) = prefix ++ "SFFMagazine" ++ num


format :: [(String, ([String] -> String))] -> String -> String
format patterns author = formatter $ fromJust matchResult
   where
      (matchResult, formatter) =
         foldr f (Nothing, const "") mkMatchExprs

      f (Nothing, _) y = y
      f x            _ = x

      mkMatchExprs =
         map (\(re, i) -> (matchRegex (mkRegex re) author, i))
            patterns


authorPatterns =
   [ ( "Dell Magazine.*", const "" )
   , ( ".* Authors", const "" )
   , ( "Spilogale.*", const "" )
   , ( "Vander Neut Publications.*", const "" )
   , ( "Crystalline Sphere Publishing.*", const "" )
   , ( "(.*) ([^ ]+) and (.*) ([^ ]+)", authorDouble )
   , ( "(.*)(Anonymous)", authorSingle )
   , ( "(.*) ([^ ]+ III)$", authorSingle )
   , ( "(.*) ([^ ]+ Jr\\.)$", authorSingle )
   , ( "(.*) (St\\. [^ ]+)$", authorSingle )
   , ( "(.*) ([^ ]+)$", authorSingle )
   ]


titlePatterns =
   [ ( "^(FSF).* ([^ ]+) ([0-9]{4})$", titleFsfMag )
   , ( "^A[eE]on ([^ ]+)$", titleAeonMag )
   , ( "^(Interzone)[^0-9]*([0-9]+)$", titleMagInterzone )
   , ( "(.*) ([^ ]+) ([0-9]{4})$", titleMagYM )
   , ( "(.*)", titleSimple )
   ]


main :: IO ()
main = do
   paths <- getArgs

   results <- mapM parseFile paths
   --mapM_ (either putStrLn ((mapM_ print) . toList)) results
   --mapM_ (either putStrLn print) results

   --let good = catRights results
   --mapM_ display2 good

   --mapM_ display3 results

   --mapM_ (\p -> parseFile p >>= display3) paths
   mapM_ (\p -> parseFile p >>= display4) paths
