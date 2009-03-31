{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.Util
   ( filterCommon, format, authorSingle, titleSimple )
   where

import Control.Monad.Error
import Data.Char
import Data.List ( foldl' )
import Data.Map hiding ( filter, map )
import Prelude hiding ( last, lookup )
import Text.Printf
import Text.Regex

import BookName.Util ( Fields )


lookupE :: (MonadError String m) => String -> Map String a -> m a
lookupE k m = case (lookup k m) of
   Nothing -> throwError $ "[ERROR missing key: " ++ k ++ "]"
   Just v -> return v


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
   [ repl "[.',\\?();#:]" ""
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


{- Pull from a (Just String) the first occurrance of a 4-digit 
   year-shaped substring. Or evaluate to ""
   Successful year strings will be prefixed like: "_2009", which
   is the format we need them in generated file paths.
-}
extractYear :: Maybe String -> String
extractYear Nothing   = ""
extractYear (Just ft) =
   case (matchRegex (mkRegex "[^0-9]*([0-9]{4}).*") ft) of
      Just (year:_) -> '_' : year
      _             -> ""


formatAuthor :: (MonadError String m) =>
               String
               -> ([String] -> String)
               -> String
               -> m String
formatAuthor re f s = case matchRegex (mkRegex re) s of
   Just xs -> return $ f xs
   Nothing -> throwError "formatAuthor failed"


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
format :: (MonadError String m) =>
          String
          -> String
          -> ([String] -> String)
          -> String
          -> (String -> [String] -> String)
          -> Fields
          -> m (String, String)
format label authorPat authorFmt titlePat titleFmt fs = do
   oldAuthor <- lookupE "Author" fs
   newAuthor <- formatAuthor authorPat authorFmt oldAuthor

   oldTitle <- lookupE "Title" fs
   let year = extractYear $ lookup "FreeText" fs
   newTitle <- formatTitle titlePat titleFmt year oldTitle

   return
      ( label
      , printf "%s%s.lrf" newAuthor newTitle
      )


{- A common simple formatter for many book authors
-}
authorSingle :: [String] -> String
authorSingle (rest:last:_) =
   printf "%s%s-" (filterCommon last) (filterCommon rest)
authorSingle _             = undefined


{- A common simple formatter for many book titles. Handles year too.
-}
titleSimple :: String -> [String] -> String
titleSimple year (old:_) = printf "%s%s" (filterCommon old) year
titleSimple _ _          = undefined
