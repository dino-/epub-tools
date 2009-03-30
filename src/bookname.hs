{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Error
import Data.Map hiding ( filter, map, null )
import Data.Maybe ( fromJust, fromMaybe )
import Prelude hiding ( lookup )
import System.Environment ( getArgs )
import System.IO ( BufferMode ( NoBuffering )
                 , hSetBuffering, stdout, stderr 
                 )
import System.Posix.Files ( rename )
import Text.Printf

import BookName.Extract ( parseFile )
import BookName.Format.Magazine ( formatMagazine )
import BookName.Format.AuthorBasic ( fmtAuthorBasic )
import BookName.Opts ( Options (..), parseOpts, usageText )
import BookName.Util ( Fields, runBN )


formatters :: [Fields -> ErrorT String IO String]
formatters = [ fmtAuthorBasic ]


lookupErrMsg :: String -> Fields -> String
lookupErrMsg k m = fromMaybe 
   ("[ERROR missing key: " ++ k ++ "]")
   $ lookup k m


{- Format a set of book fields into a line containing author, title,
   and FreeText
   This is used for verbose output.
-}
formatATF :: (PrintfType u) => Fields -> u
formatATF fields = printf "\n   %s | %s | %s"
   (lookupErrMsg "Author" fields)
   (lookupErrMsg "Title" fields)
   (lookupErrMsg "FreeText" fields)


{- Format a set of book fields into a line containing the FreeText
   This is used for verbose output.
-}
formatF :: Fields -> String
formatF fields = "\n   " ++ (lookupErrMsg "FreeText" fields)


{- Format a line of output for a book that was processed
-}
makeOutput :: Options -> Fields -> String -> String
makeOutput opts fields newPath =
   oldPath ++ " -> " ++ newPath ++ 
      ((additional (optVerbose opts)) fields)
   where
      oldPath = fromJust $ lookup "File" fields
      additional Nothing  = const ""
      additional (Just 1) = formatF
      additional _        = formatATF


{- Process an individual LRF book file
-}
processBook :: Options -> ErrorT String IO Fields -> IO ()
processBook opts parseFileAction = do
   result <- runBN $ do
      fields <- parseFileAction
      let oldPath = fromJust $ lookup "File" fields
      --liftIO $ print fields
      newPath <- foldr mplus 
         (throwError "[ERROR No formatter found]") $
         map (\f -> f fields) formatters
      unless (optNoAction opts) $ liftIO $ rename oldPath newPath
      return (fields, newPath)

   let report = either
         id
         (\(fields, newPath) -> 
            makeOutput opts fields newPath)
         result

   putStrLn report


main :: IO ()
main = do
   -- No buffering, it messes with the order of output
   mapM_ (flip hSetBuffering NoBuffering) [ stdout, stderr ]

   (opts, paths) <- getArgs >>= parseOpts

   if ((optHelp opts) || (null paths))
      then putStrLn usageText
      else do
         when (optNoAction opts) (putStrLn "No-action specified")
         let parseFileActions = map parseFile paths
         mapM_ (processBook opts) parseFileActions
