module BookName.Format.Simple
   where

import Control.Monad
import Data.Map hiding ( map )
import Data.Maybe
import Prelude hiding ( lookup )
import Text.Regex

import BookName


newtype Simple = Simple Fields


instance Format Simple where
   formatAuthor (Simple fs) =
      liftM formatAuthor' $ lookupE "Author" fs
   formatTitle (Simple fs) = do
      let year = extractYear $ lookup "FreeText" fs
      liftM (formatTitle' year) $ lookupE "Title" fs


authorSingle :: [String] -> String
authorSingle (origRest:origLast:_) = newLast ++ newRest ++ "-"
   where
      newLast = foldl (flip id) origLast commonFilters
      newRest = foldl (flip id) origRest commonFilters
authorSingle _ = undefined


authorDouble :: [String] -> String
authorDouble (_:last1:_:last2:_) = last1' ++ "_" ++ last2' ++ "-"
   where
      last1' = foldl (flip id) last1 commonFilters
      last2' = foldl (flip id) last2 commonFilters
authorDouble _ = undefined


formatAuthor' :: String -> String
formatAuthor' author = formatter $ fromJust matchResult
   where
      (matchResult, formatter) =
         foldr f (Nothing, const "") mkMatchExprs

      f (Nothing, _) y = y
      f x            _ = x

      mkMatchExprs =
         map (\(re, i) -> (matchRegex (mkRegex re) author, i))
            authorPatterns

      authorPatterns :: [(String, [String] -> String)]
      authorPatterns =
         [ ( "(.*) ([^ ]+) and (.*) ([^ ]+)", authorDouble )
         , ( "(.*)(Anonymous)", authorSingle )
         , ( "(.*) ([^ ]+ III)$", authorSingle )
         , ( "(.*) ([^ ]+ Jr\\.)$", authorSingle )
         , ( "(.*) (St\\. [^ ]+)$", authorSingle )
         , ( "(.*) ([^ ]+)$", authorSingle )
         ]


titleSimple :: String -> [String] -> String
titleSimple year (old:_) = (foldl (flip id) old commonFilters) ++ year
titleSimple _ _ = undefined


formatTitle' :: String -> String -> String
formatTitle' year author = formatter year $ fromJust matchResult
   where
      (matchResult, formatter) =
         foldr f (Nothing, (\_ _ -> "")) mkMatchExprs

      f (Nothing, _) y = y
      f x            _ = x

      mkMatchExprs =
         map (\(re, i) -> (matchRegex (mkRegex re) author, i))
            titlePatterns

      titlePatterns :: [(String, String -> [String] -> String)]
      titlePatterns =
         [ ( "(.*)", titleSimple )
         ]
