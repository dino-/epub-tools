-- Copyright: 2008-2012 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

module EpubTools.EpubName.Format.Format
   ( ReplF
   , authors, index, literal, monthNum, pad, scrub, wordNum, year
   , Formatter (..)
   , ordinaryBookFormatter
   , tryFormatting
   , extractTitle
   )
   where

import Codec.Epub.Opf.Package
import Control.Monad.Error
import Data.List ( isPrefixOf )
import Text.Printf
import Text.Regex

import EpubTools.EpubName.Format.Author
import EpubTools.EpubName.Format.PubYear
import EpubTools.EpubName.Format.Util
import EpubTools.EpubName.Opts


type ReplF = [String] -> EN String


data Formatter = Formatter
   String         -- formatter label
   (EN ())        -- author match pattern
   (EN [String])  -- title pattern parser
   [ReplF]        -- name building actions


{- A single, simple formatter that should handle most books with one or
   more author.
-}
ordinaryBookFormatter :: Formatter
ordinaryBookFormatter = Formatter
   "ordinary_book"
   (return ())
   (extractTitle ".*")
   [authors, scrub "1", year]


readE :: (MonadError String m, Read a) => String -> String -> m a
readE msg s = case reads s of
   [(x, "")] -> return x
   _         -> throwError msg


readIntE :: MonadError String m => String -> m Int
readIntE = readE "Not a number"


elemE :: MonadError String m => [a] -> Int -> m a
elemE l i
   | i < length l = return $ l !! i
   | otherwise    = throwError $ "Bad array index: " ++ show i


authors :: ReplF
authors _ = extractAuthors
   -- WARNING extractAuthors appends - to the string, it should not


index :: String -> ReplF
index sIndex matches = readIntE sIndex >>= return . subtract 1
   >>= elemE matches


literal :: String -> ReplF
literal s = return . const s


monthNum :: String -> ReplF
monthNum sIndex matches = index sIndex matches >>= monthStrToNumbers


pad :: String -> String -> ReplF
pad sWidth sIndex matches = do
   width <- readIntE sWidth
   value <- index sIndex matches >>= readIntE
   return $ printf "%0*d" (width :: Int) (value :: Int)


scrub :: String -> ReplF
scrub sIndex matches = fmap filterCommon $ index sIndex matches


wordNum :: String -> String -> ReplF
wordNum sWidth sIndex matches = do
   value <- index sIndex matches >>= numStrToInt
   width <- readIntE sWidth
   return $ printf "%0*d" (width :: Int) (value :: Int)


year :: ReplF
year _ = getPubYear
   -- WARNING getPutYear prepends _ to the string, it should not


name :: [String] -> [ReplF] -> EN String
name matches replacers =
   fmap concat $ sequence $ map (\a -> a matches) replacers


tryFormatter :: Formatter -> EN (String, FilePath)
tryFormatter (Formatter label authorMatch titlePat nameBuilders) = do
   authorMatch
   matches <- titlePat
   newName <- name matches nameBuilders
   return (label, newName)


tryFormatting :: [Formatter] -> FilePath -> EN (String, String)
tryFormatting formatters oldPath = do
   foldr mplus
      (throwError $ printf "%s [ERROR No formatter found]" oldPath) $
      map tryFormatter formatters


{-
addPublisher :: (String, [String]) -> EN (String, String)
addPublisher (label, parts) = do
   md <- asks gMetadata

   publisher <- fmap (extractPublisher md) $ asks $ optPublisher . gOpts
   return ( label
      , foldr1 (++) (parts ++ [publisher, ".epub"]))
-}


extractTitle :: String -> EN [String]
extractTitle re = do
   md <- asks gMetadata

   (MetaTitle _ oldTitle) <- case metaTitles md of
      [] -> throwError "format failed, no title present"
      ts -> return . head $ ts

   case matchRegex (mkRegex re) oldTitle of
      Just matches -> return matches
      Nothing      -> throwError $ printf "extract title failed: %s" re


extractPublisher :: Metadata -> Bool -> String
extractPublisher _  False = ""
extractPublisher md True  = maybe "" ('_' :)
   (foldr mplus Nothing (map maybePub $ metaContributors md))

   where
      maybePub (MetaCreator (Just "bkp") (Just fa) _ ) = Just fa
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
monthStrToNumbers s
   | isPrefixOf s "October"            = return "10"
monthStrToNumbers "October-November"   = return "10_11"
monthStrToNumbers "October/November"   = return "10_11"
monthStrToNumbers "Oct-Nov"            = return "10_11"
monthStrToNumbers "Oct/Nov"            = return "10_11"
monthStrToNumbers "November-December"  = return "11_12"
monthStrToNumbers s
   | isPrefixOf s "November"           = return "11"
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
