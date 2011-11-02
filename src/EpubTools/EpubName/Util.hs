-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubName.Util
   ( EN , runEN
   , throwError
   , asks
   , filterCommon
   )
   where

import Control.Monad.Error
import Control.Monad.Reader
import Data.Char
import Data.List ( foldl' )
import Text.Regex

import EpubTools.EpubName.Opts


type EN a = ReaderT Options (ErrorT String IO) a

runEN :: Options -> EN a -> IO (Either String a)
runEN env ev = runErrorT $ runReaderT ev env


{- Convenience function to make a regex replacer for a given pattern 
   and replacement string. Helps by doing the 'flip' of str and rpl
   so you can partial eval.
-}
repl :: String -> String -> String -> String
repl re rpl str = subRegex (mkRegex re) str rpl


{- Transforms a string like this:
      "the quick brown McCheeseburger" -> "TheQuickBrownMccheeseburger"
-}
capFirstAndDeSpace :: String -> String
capFirstAndDeSpace s = concat $ map capFirst $ words s
   where
      capFirst (first:rest) = (toUpper first) : (map toLower rest)
      capFirst _ = undefined


{- A set of common string filters that apply to any and all parts
   of every single string we process in this project.
-}
commonFilters :: [(String -> String)]
commonFilters =
   [ repl "[',\\?();#’]"  ""
   , repl "\\."           " "
   , repl ":"             "_"
   , filter (/= '"')
   , repl "]"             ""
   , repl "\\*"           ""
   , repl "!"             ""
   , repl "-"             " "
   , repl "\\["           "_ "
   -- Decided that I like the article included in titles
   --, repl "^The "         ""
   , repl "&"             " And "
   , capFirstAndDeSpace
   ]


{- Utility function to apply the above commonFilters to a string,
   giving you back the transformed string
-}
filterCommon :: String -> String
filterCommon s = foldl' (flip id) s commonFilters
