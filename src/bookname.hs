#! /usr/bin/env runhaskell

{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Error
import Data.List hiding ( lookup )
import Data.Map hiding ( filter, map, null )
import Data.Maybe
import HSH.Command
import Prelude hiding ( lookup )
import System.Environment
import System.FilePath
import System.Posix.Files ( rename )
import Text.Printf
import Text.Regex

import BookName
import BookName.Format.Simple
import BookName.Opts


--type BN a = (ErrorT String IO) a


runBN :: (ErrorT e m) a -> m (Either e a)
runBN = runErrorT


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


titleMagAeon :: String -> [String] -> String
titleMagAeon _ (numWord:_) = "AeonMagazine" ++ (num numWord)
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
titleMagAeon _ _ = undefined


titleFsfMag :: String -> [String] -> String
titleFsfMag _ (prefix:rest) =
   titleMagYM "foo" ((prefix ++ "Magazine"):rest)
titleFsfMag _ _ = undefined


titleMagYM :: String -> [String] -> String
titleMagYM _ (prefix:month:year:_) =
   prefix' ++ year ++ "-" ++ (monthNum month)
   where
      prefix' = foldl (flip id) prefix commonFilters
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
titleMagYM _ _ = undefined


titleMagInterzone :: String -> [String] -> String
titleMagInterzone _ (prefix:num:_) = prefix ++ "SFFMagazine" ++ num
titleMagInterzone _ _ = undefined


{-
formatAuthor :: String -> String
formatAuthor author = formatter $ fromJust matchResult
   where
      (matchResult, formatter) =
         foldr f (Nothing, const "") mkMatchExprs

      f (Nothing, _) y = y
      f x            _ = x

      mkMatchExprs =
         map (\(re, i) -> (matchRegex (mkRegex re) author, i))
            authorPatterns

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


formatTitle :: String -> String -> String
formatTitle year author = formatter year $ fromJust matchResult
   where
      (matchResult, formatter) =
         foldr f (Nothing, (\_ _ -> "")) mkMatchExprs

      f (Nothing, _) y = y
      f x            _ = x

      mkMatchExprs =
         map (\(re, i) -> (matchRegex (mkRegex re) author, i))
            titlePatterns

      titlePatterns :: [(String, String -> [String] -> String)]
      titlePatterns =
         [ ( "^(FSF).* ([^ ]+) ([0-9]{4})$", titleFsfMag )
         , ( "^A[eE]on ([^ ]+)$", titleMagAeon )
         , ( "^(Interzone)[^0-9]*([0-9]+)$", titleMagInterzone )
         , ( "(.*) ([^ ]+) ([0-9]{4})$", titleMagYM )
         , ( "(.*)", titleSimple )
         ]
-}


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


makeOutput :: Options -> Fields -> String -> String -> String
makeOutput opts fields oldPath newPath =
   oldPath ++ " -> " ++ newPath ++ 
      ((additional (optVerbose opts)) fields)
   where
      additional Nothing  = const ""
      additional (Just 1) = formatF
      additional _        = formatATF


formatters :: [Fields -> ErrorT String IO String]
formatters = [ formatSimple ]


{- Process an individual LRF book file
-}
processBook :: Options -> FilePath -> IO ()
processBook opts oldPath = do
   result <- runBN $ do
      fields <- parseFile oldPath
      newPath <- foldr mplus 
         (throwError "No suitable formatter found!") $
         map (\f -> f fields) formatters
      unless (optNoAction opts) $ liftIO $ rename oldPath newPath
      return (fields, newPath)

   let report = either
         (\errmsg -> addPath errmsg)
         (\(fields, newPath) -> 
            makeOutput opts fields oldPath newPath)
         result

   putStrLn report

   where
      addPath :: String -> String
      addPath s = printf "%s (%s)" s oldPath


main :: IO ()
main = do
   (opts, paths) <- getArgs >>= parseOpts

   if ((optHelp opts) || (null paths))
      then putStrLn usageText
      else do
         when (optNoAction opts) (putStrLn "No-action specified")
         mapM_ (processBook opts) paths
