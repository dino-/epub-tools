#! /usr/bin/env runhaskell

{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Error
import Data.Char
import Data.Either
import Data.List hiding ( lookup )
import Data.Map hiding ( filter, map )
import Data.Maybe
import HSH.Command
import Prelude hiding ( lookup )
import System.Environment
import System.FilePath
import Text.Printf
import Text.Regex


type LR a = (ErrorT String IO) a


runLR :: ErrorT e m a -> m (Either e a)
runLR = runErrorT


type Fields = Map String String


{- Anybody need this?
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
-}


parseLine :: String -> Maybe (String, String)
parseLine line =
   case (fromJust $ matchRegex (mkRegex "(.*): (.*)") line) of
      [_  , "" ] -> Nothing
      [key, val] -> Just (key, val)


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
      Left ps -> throwError $ 
         "[ERROR File " ++ path ++ " is probably not an LRF file]"
      Right output -> return output


parseFile :: (MonadIO m, MonadError String m) => String -> m Fields
parseFile path = do
   output <- extractMeta path
   return $ parseMeta path output


displayVerbose :: (PrintfArg t, PrintfType u) => Fields -> t -> u
displayVerbose fs newPath = printf "%s | %s | %s"
   newPath
   (lookupErrMsg "Author" fs)
   (lookupErrMsg "Title" fs)

   where
      lookupErrMsg k m = fromMaybe 
         ("[ERROR displayVerbose: missing key: " ++ k ++ "]")
         $ lookup k m


constructNewPath :: (MonadError String m) => Fields -> m String
constructNewPath fs = do
   newAuthor <- liftM (format authorPatterns) $ lookupE "Author" fs
   newTitle <- liftM (format titlePatterns) $ lookupE "Title" fs
   return $ newAuthor ++ newTitle ++ ".lrf"


lookupE :: (MonadError String m) => String -> Map String a -> m a
lookupE k m = case (lookup k m) of
   Nothing -> throwError $ "[ERROR missing key: " ++ k ++ "]"
   Just v -> return v


nameFilters :: [(String -> String)]
nameFilters =
   [ (\s -> subRegex (mkRegex "[.',\\?();#]") s "")
   , filter (/= '"')
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


processBook :: FilePath -> IO ()
processBook path = do
   result <- runLR $ do
      fs <- parseFile path
      np <- constructNewPath fs
      return (fs, np)

   let report = either
         (\errmsg -> addPath errmsg)
         (\(fields, newPath) -> addPath (displayVerbose fields newPath))
         result

   putStrLn report

   where
      addPath :: String -> String
      addPath s = printf "%s (%s)" s (takeFileName path)


main :: IO ()
main = do
   paths <- getArgs
   mapM_ processBook paths
