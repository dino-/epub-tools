{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.AuthorBasic
   ( fmtAuthorBasic )
   where

import Control.Monad.Error
import Prelude hiding ( last )
import Text.Printf

import BookName.Format.Util ( filterCommon, format, titleSimple )
import BookName.Util ( Fields )


fmtAuthorBasic :: (MonadError String m) => Fields -> m String
fmtAuthorBasic = format "(.*) ([^ ]+)$" authorSingle "(.*)" titleSimple


authorSingle :: [String] -> String
authorSingle (rest:last:_) = 
   printf "%s%s-" (filterCommon last) (filterCommon rest)
authorSingle _             = undefined


{-
authorDouble :: [String] -> String
authorDouble (last1:last2:_) = last1' ++ "_" ++ last2' ++ "-"
   where
      last1' = foldl (flip id) last1 commonFilters
      last2' = foldl (flip id) last2 commonFilters
authorDouble _ = undefined


authorPatterns :: [(String, [String] -> String)]
authorPatterns =
   [ ( ".* ([^ ]+) and .* ([^ ]+)", authorDouble )
   , ( "(.*)(Anonymous)", authorSingle )
   , ( "(.*) ([^ ]+ III)$", authorSingle )
   , ( "(.*) ([^ ]+ Jr\\.)$", authorSingle )
   , ( "(.*) (St\\. [^ ]+)$", authorSingle )
   , ( "(.*) ([^ ]+)$", authorSingle )
   ]
-}
