{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.Simple
   ( formatSimple )
   where

import Control.Monad.Error
import Data.Map hiding ( map )
import Prelude hiding ( lookup )

import BookName.Format.Util ( commonFilters, extractYear, 
   formatAuthor, formatTitle, lookupE )
import BookName.Util


formatSimple :: (MonadError String m) => Fields -> m String
formatSimple fs = do
   newAuthor <- lookupE "Author" fs >>= formatAuthor authorPatterns
   let year = extractYear $ lookup "FreeText" fs
   newTitle <- lookupE "Title" fs >>= formatTitle titlePatterns year
   return $ newAuthor ++ newTitle ++ ".lrf"


authorSingle :: [String] -> String
authorSingle (origRest:origLast:_) = newLast ++ newRest ++ "-"
   where
      newLast = foldl (flip id) origLast commonFilters
      newRest = foldl (flip id) origRest commonFilters
authorSingle _ = undefined


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


titleSimple :: String -> [String] -> String
titleSimple year (old:_) = (foldl (flip id) old commonFilters) ++ year
titleSimple _ _ = undefined


titlePatterns :: [(String, String -> [String] -> String)]
titlePatterns =
   [ ( "(.*)", titleSimple )
   ]
