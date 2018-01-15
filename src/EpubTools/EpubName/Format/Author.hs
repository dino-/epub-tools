-- License: ISC (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubName.Format.Author
   ( extractAuthors
   , authorMatches
   )
   where

import Codec.Epub.Data.Metadata
import Control.Monad
import Data.List ( intercalate )
import Data.Maybe ( isJust )
import Prelude hiding ( last )
import Text.Printf
import Text.Regex

import EpubTools.EpubName.Format.Util


extractAuthors :: EN String
extractAuthors = do
   fmap (fmtAuthor . separateCombined . justAuthors) $ asks gMetadata
   where
      fmtAuthor []  = ""
      fmtAuthor [c] = formatSingleAuthor c
      fmtAuthor cs = (formatMultiAuthors cs) ++ "-"


separateCombined :: [Creator] -> [Creator]
separateCombined = concatMap separateOne


separateOne :: Creator -> [Creator]
separateOne c@(Creator r _ _ di) =
   case (splitRegex (mkRegex " (&|and) ") di) of
      -- If there was only one, leave the file-as alone!
      [_] -> [c]
      -- Otherwise, explode them into separate CreatorS
      ss  -> map (Creator r Nothing Nothing) ss


formatSingleAuthor :: Creator -> String

formatSingleAuthor (Creator _ (Just fa) _ di) = 
   if ((fa == di) && all (/= ',') fa)
      then formatSingleAuthor $ Creator Nothing Nothing Nothing di
      else authorSingle [fa]

formatSingleAuthor (Creator _ _         _ di) = 
   authorSingle . reverse . nameParts $ di


nameParts :: String -> [String]
nameParts s = maybe [] id $ foldl mplus Nothing matches
   where
      matches = map ($ s)
         [ matchRegex (mkRegex "(.*), +([^ ]+)$") >=> return . reverse
         , matchRegex (mkRegex "(.*) +([^ ]+)$")
         , matchRegex (mkRegex "(.*)")
         ]


lastName' :: String -> String
lastName' s = maybe "" head $ foldl mplus Nothing matches
   where
      matches = map ($ s)
         [ matchRegex (mkRegex "(.*),.*")
         , matchRegex (mkRegex ".* (.*)")
         , matchRegex (mkRegex "(.*)")
         ]

lastName :: Creator -> String
lastName (Creator _ (Just fa) _ _ ) = lastName' fa
lastName (Creator _ _         _ di) = lastName' di


formatMultiAuthors :: [Creator] -> String
formatMultiAuthors = (intercalate "_") . (map lastName)


justAuthors :: Metadata -> [Creator]
justAuthors = (filter isAut) . metaCreators
   where
      isAut (Creator (Just "aut") _ _ _) = True
      isAut (Creator Nothing      _ _ _) = True
      isAut _                            = False


{- A common simple formatter for many book authors
-}
authorSingle :: [String] -> String
authorSingle (last:rest:_) =
   printf "%s%s-" (scrubString last) (scrubString rest)
authorSingle [n]           =
   printf "%s-" $ scrubString $ takeWhile (/= '(') n
authorSingle _             = undefined


{- Throws an error if no author matches the pattern
-}
authorMatches :: String -> EN ()
authorMatches re = do
   let authorMatches' (Creator _ _ _ di) =
         matchRegex (mkRegex re) di

   md <- asks gMetadata
   unless (any isJust $ map authorMatches' $ justAuthors md) $ throwError ""


{- Author names with a postfix like II, III, Jr. or Sr.
   FIXME: Is anyone calling this?
-}
{-
authorPostfix :: [String] -> String
authorPostfix (rest:last:postfix:_) =
   printf "%s%s%s-" (filterCommon last) (filterCommon rest)
      (filterCommon postfix)
authorPostfix _             = undefined
-}
