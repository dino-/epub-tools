{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.Util
   ( lookupE
   , commonFilters
   , extractYear
   )
   where

import Control.Monad.Error
import Data.Char
import Data.Map hiding ( filter, map )
import Data.Maybe
import Prelude hiding ( lookup )
import Text.Regex


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


{- Pull from a (Just String) the first occurrance of a 4-digit 
   year-shaped substring. Or evaluate to ""
   Successful year strings will be prefixed like: "_2009", which
   is the format we need them in generated file paths.
-}
extractYear :: Maybe String -> String
extractYear Nothing   = ""
extractYear (Just ft) =
   case (matchRegex (mkRegex ".*([0-9]{4}).*") ft) of
      Just (year:_) -> '_' : year
      _             -> ""


{-
formatAuthor :: (MonadError String m) =>
   [(String, [String] -> a)] -> String -> m a
formatAuthor authorPatterns author = do
   let (matchResult, formatter) =
         foldr f (Nothing, Nothing) mkMatchExprs
   case formatter of
      Nothing -> throwError "No handler found"
      Just g -> return $ g $ fromJust matchResult

   where
      f (Nothing, _) y = y
      f x            _ = x

      mkMatchExprs =
         map (\(re, i) -> (matchRegex (mkRegex re) author, Just i))
            authorPatterns


formatTitle :: (MonadError String m) => 
   [(String, String -> [String] -> a)] -> String -> String -> m a
formatTitle titlePatterns year author = do
   let (matchResult, formatter) =
         foldr f (Nothing, Nothing) mkMatchExprs
   case formatter of
      Nothing -> throwError "No handler found"
      Just g -> return $ g year $ fromJust matchResult

   where
      f (Nothing, _) y = y
      f x            _ = x

      mkMatchExprs =
         map (\(re, i) -> (matchRegex (mkRegex re) author, Just i))
            titlePatterns
-}
