{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.Util
   ( filterCommon, format
   , author
   , authorSingle, authorPostfix
   , titleSimple
   , monthNum )
   where

import Codec.Epub.Opf.Package
import Control.Monad.Error
import Data.Char
import Data.List ( foldl', intercalate, isPrefixOf )
import Data.Maybe ( fromJust )
import Prelude hiding ( last )
import Text.Printf
import Text.Regex



{- Convenience function to make a regex replacer for a given pattern 
   and replacement string. Helps by doing the 'flip' of str and rpl
   so you can partial eval.
-}
repl :: String -> String -> String -> String
repl re rpl str = subRegex (mkRegex re) str rpl


{- Transforms a string like this:
      "the quick brown fox" -> "TheQuickBrownFox"
-}
capFirstAndDeSpace :: String -> String
capFirstAndDeSpace s = concat $ map capFirst $ words s
   where
      capFirst (first:rest) = (toUpper first) : rest
      capFirst _ = undefined


{- A set of common string filters that apply to any and all parts
   of every single string we process in this project.
-}
commonFilters :: [(String -> String)]
commonFilters =
   [ repl "[.',\\?();#]" ""
   , repl ":"             "_"
   , filter (/= '"')
   , repl "]"             ""
   , repl "\\*"           ""
   , repl "!"             ""
   , repl "-"             " "
   , repl "\\["           "_"
   , repl "^The "         ""
   , repl "&"             " And "
   , capFirstAndDeSpace
   ]


{- Utility function to apply the above commonFilters to a string,
   giving you back the transformed string
-}
filterCommon :: String -> String
filterCommon s = foldl' (flip id) s commonFilters


{- Look for a date tag with event="original-publication" in the
   metadata
-}
extractYear :: Metadata -> String
extractYear md = maybe "" ('_' :)
   (foldr mplus Nothing (map maybeYear $ metaDates md))

   where
      maybeYear (MetaDate (Just "original-publication") d) = Just d
      maybeYear _                                          = Nothing


{- Convert an English month name (with creative ranges and variations)
   into number form
-}
monthNum :: String -> String
monthNum x
   | isPrefixOf x "January"   = "01"
monthNum "January-February"   = "01_02"
monthNum "January/February"   = "01_02"
monthNum "Jan-Feb"            = "01_02"
monthNum "Jan/Feb"            = "01_02"
monthNum x
   | isPrefixOf x "February"  = "02"
monthNum x
   | isPrefixOf x "March"     = "03"
monthNum "March-April"        = "03_04"
monthNum "March/April"        = "03_04"
monthNum "Mar-Apr"            = "03_04"
monthNum "Mar/Apr"            = "03_04"
monthNum x
   | isPrefixOf x "April"     = "04"
monthNum "April-May"          = "04_05"
monthNum "April/May"          = "04_05"
monthNum "Apr-May"            = "04_05"
monthNum "Apr/May"            = "04_05"
monthNum "May"                = "05"
monthNum "May-June"           = "05_06"
monthNum "May/June"           = "05_06"
monthNum "May-Jun"            = "05_06"
monthNum "May/Jun"            = "05_06"
monthNum x
   | isPrefixOf x "June"      = "06"
monthNum "June/July"          = "06_07"
monthNum "June-July"          = "06_07"
monthNum "Jun-Jul"            = "06_07"
monthNum "Jun/Jul"            = "06_07"
monthNum x
   | isPrefixOf x "July"      = "07"
monthNum "July-August"        = "07_08"
monthNum "July/August"        = "07_08"
monthNum "Jul-Aug"            = "07_08"
monthNum "Jul/Aug"            = "07_08"
monthNum x
   | isPrefixOf x "August"    = "08"
monthNum "August-September"   = "08_09"
monthNum "August/September"   = "08_09"
monthNum "Aug-Sep"            = "08_09"
monthNum "Aug/Sep"            = "08_09"
monthNum x
   | isPrefixOf x "September" = "09"
monthNum "September-October"  = "09_10"
monthNum x
   | isPrefixOf x "October"   = "10"
monthNum "October-November"   = "10_11"
monthNum "October/November"   = "10_11"
monthNum "Oct-Nov"            = "10_11"
monthNum "Oct/Nov"            = "10_11"
monthNum x
   | isPrefixOf x "November"  = "11"
monthNum x
   | isPrefixOf x "December"  = "12"
monthNum x                    = "[ERROR monthNum " ++ x ++ "]"


{-
formatAuthor :: (MonadError String m) =>
               String
               -> (Metadata -> String)
               -> String
               -> m String
formatAuthor re f s = case matchRegex (mkRegex re) s of
   Just xs -> return $ f xs
   Nothing -> throwError "formatAuthor failed"
-}
formatAuthor :: (MonadError String m) =>
               String
               -> (Metadata -> String)
               -> Metadata
               -> m String
formatAuthor re f md =
   maybe (throwError "formatAuthor failed")
      (const (return . f $ md)) tryPats

   where
      justAuthorStrings = map (\(MetaCreator _ _ n) -> n)

      match = matchRegex (mkRegex re)

      tryPats = foldr mplus Nothing
         ((map match) . justAuthorStrings . justAuthors $ md)


formatTitle :: (MonadError String m) =>
               String
               -> (String -> [String] -> String)
               -> String
               -> String
               -> m String
formatTitle re f year s = case matchRegex (mkRegex re) s of
   Just xs -> return $ f year xs
   Nothing -> throwError "formatTitle failed"


{- This is the main work performing function that's called by every 
   formatter. It expects to see a regexp pattern and format function 
   for both author and title parts of the book info. And then the map 
   of fields from a book.
   If the pattern matches, the resulting list of match results is sent 
   to the supplied format function. If any of this fails, the entire 
   action throws.
-}
{-
format :: (MonadError String m) =>
          String
          -> String
          -> ([String] -> String)
          -> String
          -> (String -> [String] -> String)
          -> Fields
          -> m (String, String)
format label authorPat authorFmt titlePat titleFmt fs = do
   oldAuthor <- lookupE "Authors" fs
   newAuthor <- formatAuthor authorPat authorFmt oldAuthor

   oldTitle <- lookupE "Title" fs
   let year = extractYear $ lookup "Comment" fs
   newTitle <- formatTitle titlePat titleFmt year oldTitle

   return
      ( label
      , printf "%s%s.epub" newAuthor newTitle
      )
-}
format :: (MonadError String m)
          => String
          -> String
          -> (Metadata -> String)
          -> String
          -> (String -> [String] -> String)
          -> Metadata
          -> m (String, String)
format label authorPat authorFmt titlePat titleFmt md = do
   newAuthor <- formatAuthor authorPat authorFmt md

   let (MetaTitle _ oldTitle) = head . metaTitles $ md
   let year = extractYear md
   newTitle <- formatTitle titlePat titleFmt year oldTitle

   return
      ( label
      , printf "%s%s.epub" newAuthor newTitle
      )


formatSingleAuthor :: MetaCreator -> String
formatSingleAuthor (MetaCreator _ (Just fa) _ ) = authorSingle parts
   where
      parts = fromJust . (matchRegex (mkRegex "(.*), (.*)")) $ fa
formatSingleAuthor (MetaCreator _ _         di) = 
   authorSingle . reverse $ parts
   where
      parts = fromJust .
         (matchRegex (mkRegex "(.*) ([^ ]+)$")) $ di


lastName :: MetaCreator -> String
lastName (MetaCreator _ (Just fa) _ ) = head . fromJust .
   (matchRegex (mkRegex "(.*),.*")) $ fa
lastName (MetaCreator _ _         di) = head . fromJust .
   (matchRegex (mkRegex ".* (.*)")) $ di


formatMultiAuthors :: [MetaCreator] -> String
formatMultiAuthors = (intercalate "_") . (map lastName)


justAuthors :: Metadata -> [MetaCreator]
justAuthors = (filter isAut) . metaCreators
   where
      isAut (MetaCreator (Just "aut") _ _) = True
      isAut (MetaCreator Nothing      _ _) = True
      isAut _                              = False


author :: Metadata -> String
author = fmtAuthor . justAuthors
   where
      fmtAuthor [c] = formatSingleAuthor c
      fmtAuthor cs = (formatMultiAuthors cs) ++ "-"


{- A common simple formatter for many book authors
-}
authorSingle :: [String] -> String
authorSingle (last:rest:_) =
   printf "%s%s-" (filterCommon last) (filterCommon rest)
authorSingle _             = undefined


{- Author names with a postfix like II, III, Jr. or Sr.
-}
authorPostfix :: [String] -> String
authorPostfix (rest:last:postfix:_) =
   printf "%s%s%s-" (filterCommon last) (filterCommon rest)
      (filterCommon postfix)
authorPostfix _             = undefined


{- A common simple formatter for many book titles. Handles year too.
-}
titleSimple :: String -> [String] -> String
titleSimple year (old:_) = printf "%s%s" (filterCommon old) year
titleSimple _ _          = undefined
