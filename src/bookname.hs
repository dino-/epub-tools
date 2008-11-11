#! /usr/bin/env runhaskell

{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Error
import Data.Char
import Data.List hiding ( lookup )
import Data.Map hiding ( filter, map )
import Data.Maybe
import HSH.Command
import Prelude hiding ( lookup )
import System.Environment
import System.FilePath
import Text.Printf
import Text.Regex


--type BN a = (ErrorT String IO) a


runBN :: ErrorT e m a -> m (Either e a)
runBN = runErrorT


type Fields = Map String String


parseLine :: String -> Maybe (String, String)
parseLine line =
   case (fromJust $ matchRegex (mkRegex "(.*): (.*)") line) of
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


capFirstAndDeSpace :: String -> String
capFirstAndDeSpace s = concat $ map capFirst $ words s
   where
      capFirst (first:rest) = (toUpper first) : rest
      capFirst _ = undefined


authorDouble :: [String] -> String
authorDouble (_:last1:_:last2:_) = last1' ++ "_" ++ last2' ++ "-"
   where
      last1' = foldl (flip id) last1 nameFilters
      last2' = foldl (flip id) last2 nameFilters
authorDouble _ = undefined


authorSingle :: [String] -> String
authorSingle (rest:last:_) = last' ++ rest' ++ "-"
   where
      last' = foldl (flip id) last nameFilters
      rest' = foldl (flip id) rest nameFilters
authorSingle _ = undefined


titleSimple :: [String] -> String
titleSimple (old:_) = foldl (flip id) old nameFilters
titleSimple _ = undefined


titleMagAeon :: [String] -> String
titleMagAeon (numWord:_) = "AeonMagazine" ++ (num numWord)
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
      num x           = "[ERROR titleMagAeon " ++ x ++ "]"
titleMagAeon _ = undefined


titleFsfMag :: [String] -> String
titleFsfMag (prefix:rest) =
   titleMagYM ((prefix ++ "Magazine"):rest)
titleFsfMag _ = undefined


titleMagYM :: [String] -> String
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
      monthNum x                    = "[ERROR titleMagYM " ++ x ++ "]"
titleMagYM _ = undefined


titleMagInterzone :: [String] -> String
titleMagInterzone (prefix:num:_) = prefix ++ "SFFMagazine" ++ num
titleMagInterzone _ = undefined


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


authorPatterns :: [(String, [String] -> String)]
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


titlePatterns :: [(String, [String] -> String)]
titlePatterns =
   [ ( "^(FSF).* ([^ ]+) ([0-9]{4})$", titleFsfMag )
   , ( "^A[eE]on ([^ ]+)$", titleMagAeon )
   , ( "^(Interzone)[^0-9]*([0-9]+)$", titleMagInterzone )
   , ( "(.*) ([^ ]+) ([0-9]{4})$", titleMagYM )
   , ( "(.*)", titleSimple )
   ]


lookupErrMsg :: String -> Fields -> String
lookupErrMsg k m = fromMaybe 
   ("[ERROR missing key: " ++ k ++ "]")
   $ lookup k m


formatATF :: (PrintfType u) => Fields -> u
formatATF fields = printf "\n   %s | %s | %s"
   (lookupErrMsg "Author" fields)
   (lookupErrMsg "Title" fields)
   (lookupErrMsg "FreeText" fields)


formatF :: Fields -> String
formatF fields = "\n   " ++ (lookupErrMsg "FreeText" fields)


makeOutput :: Fields -> String -> String -> String
makeOutput fields newPath oldPath =
   oldPath ++ " -> " ++ newPath ++ (additional verbose)
   where
      additional False = ""
      --additional True  = formatF fields
      additional True  = formatATF fields


{- Process an individual LRF book file
-}
processBook :: FilePath -> IO ()
processBook path = do
   -- Parse the LRF file and build new filepath as a potentially
   -- error-producing computation
   result <- runBN $ do
      fs <- parseFile path
      np <- constructNewPath fs
      return (fs, np)

   -- Turn the result of above into a displayable message for the user
   let report = either
         (\errmsg -> addPath errmsg)
         (\(fields, newPath) -> makeOutput fields newPath path)
         result

   putStrLn report

   where
      addPath :: String -> String
      addPath s = printf "%s (%s)" s path


verbose = True
--verbose = False


main :: IO ()
main = do
   paths <- getArgs
   mapM_ processBook paths
