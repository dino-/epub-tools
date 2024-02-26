-- License: ISC (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts, OverloadedRecordDot #-}

module EpubTools.EpubName.Format.Format
   ( ReplF
   , authors, index, literal, monthNum, pad, publisher, scrub, wordNum
   , year
   , Formatter (..)
   , tryFormatting
   , subjectMatches
   , extractTitle
   )
   where

import Codec.Epub.Data.Metadata
import Control.Monad.Except
import Data.List ( isPrefixOf )
import Data.Maybe ( isJust, listToMaybe )
import Text.Printf
import Text.Regex

import EpubTools.EpubName.Common
  ( Options (includePublisher)
  , PublisherSwitch (..)
  )
import EpubTools.EpubName.Format.Author
import EpubTools.EpubName.Format.PubYear
import EpubTools.EpubName.Format.Util


type ReplF = [String] -> EN String


data Formatter = Formatter
   String         -- formatter label
   [EN ()]        -- field pattern matchers
   (EN [String])  -- title pattern parser
   [ReplF]        -- name building actions


{- A single, simple formatter that should handle most books with one or
   more author.
-}
{- Not using this at the moment, but I'm reluctant to remove it. Could
   be useful someday.
ordinaryBookFormatter :: Formatter
ordinaryBookFormatter = Formatter
   "ordinary_book"
   (return ())
   (extractTitle ".*")
   [authors, scrub "1", year]
-}


{- Wrapper functions to perform some basic operations in the Except
   String monad
-}

-- general purpose read in the Except String monad
readE :: (MonadError String m, Read a) => String -> String -> m a
readE msg s = case reads s of
   [(x, "")] -> return x
   _         -> throwError msg


-- read an Int in the Except String monad
readIntE :: MonadError String m => String -> m Int
readIntE = readE "Not a number"


-- get list element at a specific index in the Except String monad
elemE :: MonadError String m => [a] -> Int -> m a
elemE l i
   | i < length l = return $ l !! i
   | otherwise    = throwError $ "Bad array index: " ++ show (i + 1)


{- Instructions in the book naming DSL. These instructions are used from
   the rules file notation, in the name "..." command
-}

-- Get formatted authors from the metadata
authors :: ReplF
authors _ = extractAuthors


-- Retrieve a specific item from the title pattern match results
index :: String -> ReplF
index sIndex matches = readIntE sIndex >>= return . subtract 1
   >>= elemE matches


-- Insert a specific string literal into the resulting name string
literal :: String -> ReplF
literal s = return . const s


{- Construct a numeric month from a variety of English months strings
   Including abbreviations and ranges
-}
monthNum :: String -> ReplF
monthNum sIndex matches = index sIndex matches >>= monthStrToNumbers


-- Pad a numeric value from the pattern match results
pad :: String -> String -> ReplF
pad sWidth sIndex matches = do
   width <- readIntE sWidth
   value <- index sIndex matches >>= readIntE
   return $ printf "%0*d" (width :: Int) (value :: Int)


-- Get the formatted publisher file-as string from the metadata
publisher :: ReplF
publisher _ = do
   md <- asks gMetadata
   opts <- asks gOpts
   pure $ extractPublisher md opts.includePublisher.v


-- Clean up an item from the pattern match results
scrub :: String -> ReplF
scrub sIndex matches = fmap scrubString $ index sIndex matches


{- Construct a numeric string from an English word for a number,
   with padding
-}
wordNum :: String -> String -> ReplF
wordNum sWidth sIndex matches = do
   value <- index sIndex matches >>= numStrToInt
   width <- readIntE sWidth
   return $ printf "%0*d" (width :: Int) (value :: Int)


-- Get the formatted publication year from the metadata
year :: ReplF
year _ = getPubYear


{- Commands in the book naming DSL. These commands are used from the
   rules file notation, in the rule blocks
-}

{- This is used by subjectMatch to construct a subject matcher for a
   specific rule. Throws an error if no subject matches the pattern.
-}
subjectMatches :: String -> EN ()
subjectMatches re = do
   subjects <- asks $ metaSubjects . gMetadata
   unless (any isJust $ map (matchRegex (mkRegex re)) subjects) $
      throwError ""


{- This is used by the titlePat command to construct a title pattern
   matcher for a specific rule
-}
extractTitle :: String -> EN [String]
extractTitle re = do
   md <- asks gMetadata

   let titleMatch =
         (matchRegex $ mkRegexWithOpts re False True) =<<
         titleText `fmap` (listToMaybe $ metaTitles md)

   maybe (throwError "") return titleMatch


{- This is used by the name command to execute a series of the DSL
   instructions above to create a new filename
-}
name :: [String] -> [ReplF] -> EN String
name matches replacers =
   fmap concat $ sequence $ map (\a -> a matches) replacers


-- Try a specific formatter
tryFormatter :: Formatter -> EN (String, FilePath)
tryFormatter (Formatter label matchers titlePat nameBuilders) = do
   sequence_ matchers
   matches <- titlePat
   newName <- name matches nameBuilders
   return (label, sanitizeString newName)


{- Try the entire list of formatters, one by one, in order, until one
   succeeds or none do
-}
tryFormatting :: (MonadIO m, MonadError String m) =>
   Globals -> [Formatter] -> FilePath -> m (String, FilePath)
tryFormatting gs fs oldPath =
   either throwError return $ runEN gs $
      foldr mplus
         (throwError $ printf "%s [ERROR No formatter found]" oldPath) $
         map tryFormatter fs


extractPublisher :: Metadata -> Bool -> String
extractPublisher _  False = ""
extractPublisher md True  = maybe "" ('_' :)
   (foldr mplus Nothing (map maybePub $ metaContributors md))

   where
      maybePub (Creator (Just "bkp") (Just fa) _ _ ) = Just fa
      maybePub _                                       = Nothing


{- Convert an English month name (with creative ranges and variations)
   into number form
-}
monthStrToNumbers :: String -> EN String
monthStrToNumbers s
   | isPrefixOf s "January"            = return "01"
monthStrToNumbers "January-February"   = return "01_02"
monthStrToNumbers "January/February"   = return "01_02"
monthStrToNumbers "Jan-Feb"            = return "01_02"
monthStrToNumbers "Jan/Feb"            = return "01_02"
monthStrToNumbers s
   | isPrefixOf s "February"           = return "02"
monthStrToNumbers s
   | isPrefixOf s "March"              = return "03"
monthStrToNumbers "March-April"        = return "03_04"
monthStrToNumbers "March/April"        = return "03_04"
monthStrToNumbers "Mar-Apr"            = return "03_04"
monthStrToNumbers "Mar/Apr"            = return "03_04"
monthStrToNumbers s
   | isPrefixOf s "April"              = return "04"
monthStrToNumbers "April-May"          = return "04_05"
monthStrToNumbers "April/May"          = return "04_05"
monthStrToNumbers "Apr-May"            = return "04_05"
monthStrToNumbers "Apr/May"            = return "04_05"
monthStrToNumbers "May"                = return "05"
monthStrToNumbers "May-June"           = return "05_06"
monthStrToNumbers "May/June"           = return "05_06"
monthStrToNumbers "May-Jun"            = return "05_06"
monthStrToNumbers "May/Jun"            = return "05_06"
monthStrToNumbers s
   | isPrefixOf s "June"               = return "06"
monthStrToNumbers "June/July"          = return "06_07"
monthStrToNumbers "June-July"          = return "06_07"
monthStrToNumbers "Jun-Jul"            = return "06_07"
monthStrToNumbers "Jun/Jul"            = return "06_07"
monthStrToNumbers s
   | isPrefixOf s "July"               = return "07"
monthStrToNumbers "July-August"        = return "07_08"
monthStrToNumbers "July/August"        = return "07_08"
monthStrToNumbers "Jul-Aug"            = return "07_08"
monthStrToNumbers "Jul/Aug"            = return "07_08"
monthStrToNumbers s
   | isPrefixOf s "August"             = return "08"
monthStrToNumbers "August-September"   = return "08_09"
monthStrToNumbers "August/September"   = return "08_09"
monthStrToNumbers "Aug-Sep"            = return "08_09"
monthStrToNumbers "Aug/Sep"            = return "08_09"
monthStrToNumbers s
   | isPrefixOf s "September"          = return "09"
monthStrToNumbers "September-October"  = return "09_10"
monthStrToNumbers "September/October"  = return "09_10"
monthStrToNumbers "Sep-Oct"            = return "09_10"
monthStrToNumbers "Sep/Oct"            = return "09_10"
monthStrToNumbers s
   | isPrefixOf s "October"            = return "10"
monthStrToNumbers "October-November"   = return "10_11"
monthStrToNumbers "October/November"   = return "10_11"
monthStrToNumbers "Oct-Nov"            = return "10_11"
monthStrToNumbers "Oct/Nov"            = return "10_11"
monthStrToNumbers s
   | isPrefixOf s "November"           = return "11"
monthStrToNumbers "November-December"  = return "11_12"
monthStrToNumbers "November/December"  = return "11_12"
monthStrToNumbers "Nov-Dec"  = return "11_12"
monthStrToNumbers "Nov/Dec"  = return "11_12"
monthStrToNumbers s
   | isPrefixOf s "December"           = return "12"
monthStrToNumbers s                    =
   throwError $ printf "[ERROR monthNum %s]" s


{- Convert an English word for a number into number form
-}
numStrToInt :: String -> EN Int
numStrToInt "One"       = return 1
numStrToInt "Two"       = return 2
numStrToInt "Three"     = return 3
numStrToInt "Four"      = return 4
numStrToInt "Five"      = return 5
numStrToInt "Six"       = return 6
numStrToInt "Seven"     = return 7
numStrToInt "Eight"     = return 8
numStrToInt "Nine"      = return 9
numStrToInt "Ten"       = return 10
numStrToInt "Eleven"    = return 11
numStrToInt "Twelve"    = return 12
numStrToInt "Thirteen"  = return 13
numStrToInt "Fourteen"  = return 14
numStrToInt "Fifteen"   = return 15
numStrToInt "Sixteen"   = return 16
numStrToInt "Seventeen" = return 17
numStrToInt "Eighteen"  = return 18
numStrToInt "Nineteen"  = return 19
numStrToInt "Twenty"    = return 20
numStrToInt s           = throwError $ printf "[ERROR wordNum %s]" s
