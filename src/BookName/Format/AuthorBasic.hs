{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.AuthorBasic
   ( fmtAuthorBasic )
   where

import Control.Monad.Error
import Data.Map hiding ( map )
import Prelude hiding ( lookup )
import Text.Regex ( matchRegex, mkRegex )

import BookName.Format.Util ( commonFilters, extractYear, lookupE )
import BookName.Util


fmtAuthorBasic :: (MonadError String m) => Fields -> m String
fmtAuthorBasic fs = do
   oldAuthor <- lookupE "Author" fs
   newAuthor <- formatAuthor "(.*) ([^ ]+)$" authorSingle oldAuthor

   oldTitle <- lookupE "Title" fs
   let year = extractYear $ lookup "FreeText" fs
   newTitle <- formatTitle "(.*)" titleSimple year oldTitle

   return $ newAuthor ++ newTitle ++ ".lrf"


formatAuthor re f s = case matchRegex (mkRegex re) s of
   Just xs -> return $ f xs
   Nothing -> throwError "formatAuthor failed"


formatTitle re f year s = case matchRegex (mkRegex re) s of
   Just xs -> return $ f year xs
   Nothing -> throwError "formatTitle failed"


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
