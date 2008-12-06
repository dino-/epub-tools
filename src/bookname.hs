#! /usr/bin/env runhaskell

{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Error
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
import BookName.Format.Magazine
import BookName.Format.Simple
import BookName.Opts


--type BN a = (ErrorT String IO) a


runBN :: (ErrorT e m) a -> m (Either e a)
runBN = runErrorT


formatters :: [Fields -> ErrorT String IO String]
formatters = [ formatMagazine, formatSimple ]


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


{- Process an individual LRF book file
-}
processBook :: Options -> FilePath -> IO ()
processBook opts oldPath = do
   result <- runBN $ do
      fields <- parseFile oldPath
      --liftIO $ print fields
      newPath <- foldr mplus 
         (throwError "[ERROR No formatter found]") $
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
