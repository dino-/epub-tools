-- Copyright: 2008-2012 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubName.Format.Author
   ( extractAuthors
   , authorMatches
   )
   where

import Codec.Epub.Opf.Package
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


separateCombined :: [MetaCreator] -> [MetaCreator]
separateCombined = concatMap separateOne


separateOne :: MetaCreator -> [MetaCreator]
separateOne c@(MetaCreator r _ di) =
   case (splitRegex (mkRegex " (&|and) ") di) of
      -- If there was only one, leave the file-as alone!
      [_] -> [c]
      -- Otherwise, explode them into separate MetaCreatorS
      ss  -> map (MetaCreator r Nothing) ss


formatSingleAuthor :: MetaCreator -> String

formatSingleAuthor (MetaCreator _ (Just fa) di) = 
   if ((fa == di) && all (/= ',') fa)
      then formatSingleAuthor $ MetaCreator Nothing Nothing di
      else authorSingle [fa]

formatSingleAuthor (MetaCreator _ _         di) = 
   authorSingle . reverse . nameParts $ di


nameParts :: String -> [String]
nameParts s = maybe [] id $ foldl mplus Nothing matches
   where
      matches =
         [ matchRegex (mkRegex "(.*) ([^ ]+)$") s
         , matchRegex (mkRegex "(.*)") s
         ]


lastName' :: String -> String
lastName' s = maybe "" head $ foldl mplus Nothing matches
   where
      matches =
         [ matchRegex (mkRegex "(.*),.*") s
         , matchRegex (mkRegex ".* (.*)") s
         , matchRegex (mkRegex "(.*)") s
         ]

lastName :: MetaCreator -> String
lastName (MetaCreator _ (Just fa) _ ) = lastName' fa
lastName (MetaCreator _ _         di) = lastName' di


formatMultiAuthors :: [MetaCreator] -> String
formatMultiAuthors = (intercalate "_") . (map lastName)


justAuthors :: Metadata -> [MetaCreator]
justAuthors = (filter isAut) . metaCreators
   where
      isAut (MetaCreator (Just "aut") _ _) = True
      isAut (MetaCreator Nothing      _ _) = True
      isAut _                              = False


{- A common simple formatter for many book authors
-}
authorSingle :: [String] -> String
authorSingle (last:rest:_) =
   printf "%s%s-" (filterCommon last) (filterCommon rest)
authorSingle [n]           = printf "%s-" $ filterCommon n
authorSingle _             = undefined


{- Throws an error if no author matches the pattern
-}
authorMatches :: String -> EN ()
authorMatches re = do
   let authorMatches' (MetaCreator _ _ di) =
         matchRegex (mkRegex re) di

   md <- asks gMetadata
   unless (any isJust $ map authorMatches' $ justAuthors md) $
      throwError "Specific author string not found"


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
