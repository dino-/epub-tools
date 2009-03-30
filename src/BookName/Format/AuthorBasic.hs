{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.AuthorBasic
   ( fmtAuthorBasic )
   where

import Control.Monad.Error

import BookName.Format.Util ( commonFilters, format )
import BookName.Util ( Fields )


fmtAuthorBasic :: (MonadError String m) => Fields -> m String
fmtAuthorBasic = format "(.*) ([^ ]+)$" authorSingle "(.*)" titleSimple


authorSingle :: [String] -> String
authorSingle (origRest:origLast:_) = newLast ++ newRest ++ "-"
   where
      newLast = foldl (flip id) origLast commonFilters
      newRest = foldl (flip id) origRest commonFilters
authorSingle _ = undefined


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


titleSimple :: String -> [String] -> String
titleSimple year (old:_) = (foldl (flip id) old commonFilters) ++ year
titleSimple _ _ = undefined


{-
titlePatterns :: [(String, String -> [String] -> String)]
titlePatterns =
   [ ( "(.*)", titleSimple )
   ]
-}
