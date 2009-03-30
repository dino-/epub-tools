{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.AuthorBasic
   ( fmtAuthorBasic )
   where

import Control.Monad.Error

import BookName.Format.Util
   ( format
   , authorSingle, titleSimple
   )
import BookName.Util ( Fields )


fmtAuthorBasic :: (MonadError String m) => Fields -> m String
fmtAuthorBasic = format
   "(.*) ([^ ]+)$" authorSingle
   "(.*)" titleSimple


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
