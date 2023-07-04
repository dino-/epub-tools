-- License: ISC (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubName.Format.Util
   ( Globals (..)
   , EN , runEN
   , throwError
   , asks
   , scrubString
   , sanitizeString
   )
   where

import Codec.Epub.Data.Metadata
import Codec.Epub.Data.Package
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Char
import Data.List ( foldl' )
import Text.Regex

import EpubTools.EpubName.Opts


data Globals = Globals
   { gOpts :: Options
   , gPackage :: Package
   , gMetadata :: Metadata
   }

type EN a = ReaderT Globals (ExceptT String Identity) a

runEN :: Globals -> EN a -> Either String a
runEN env ev = runIdentity (runExceptT (runReaderT ev env))


{- Convenience function to make a regex replacer for a given pattern 
   and replacement string. Helps by doing the 'flip' of str and rpl
   so you can partial eval.
-}
repl :: String -> String -> String -> String
repl re rpl str = subRegex (mkRegex re) str rpl


{- Transforms a string like this:
      "the quick brown McCheeseburger" -> "TheQuickBrownMccheeseburger"
-}
camelCase :: String -> String
camelCase s = concat $ map capFirst $ words s
   where
      capFirst s'@('I':'I':_) = map toUpper s'
      capFirst s'@('I':'V':_) = map toUpper s'
      capFirst s'@('I':'X':_) = map toUpper s'
      capFirst (first:rest) = (toUpper first) : (map toLower rest)
      capFirst _ = undefined


{- A set of common string filters that apply to any and all parts
   of every single string we process in this project.
-}
aggressiveFilters :: [(String -> String)]
aggressiveFilters =
   [ repl "[',\\?();#’\\*!]"  ""
   , repl "]"                 ""    -- Tricky to put this in [ ]
   , repl "[./]"              " "
   , repl ":"                 "_"
   , repl "[\\[\\\n]"         "_ "
   , repl "-"                 "- "
   , filter (/= '"')
   -- Decided that I like the article included in titles
   --, repl "^The "             ""
   , repl "&"                 " And "
   , camelCase
   ]


{- Utility function to apply the above filters to a string, giving you
   back the transformed string
-}
scrubString :: String -> String
scrubString s = foldl' (flip id) s aggressiveFilters


{- Utility function to apply a very basic filter to make a legal filename,
   giving you back the transformed string. This is used on everything
   at the very end, just before the new filename is returned from the
   formatting machinery.
-}
sanitizeString :: String -> String
sanitizeString s = foldl' (flip id) s [ repl "/" " " ]
