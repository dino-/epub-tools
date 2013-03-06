-- Copyright: 2011-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubName.Format.PubYear
   ( extractYear
   , getPubYear
   )
   where

import Codec.Epub.Opf.Package
import Control.Monad
import Text.Regex

import EpubTools.EpubName.Opts
import EpubTools.EpubName.Format.Util


{- Look for publication date, return "" if none found
-}
getPubYear :: EN String
getPubYear = do
   yearHandling <- asks $ optPubYear . gOpts
   md <- asks gMetadata

   case yearHandling of
      Publication -> getPubYear' md pubAttrs
      AnyDate -> getPubYear' md $ pubAttrs ++ [getFirstDate]
      NoDate -> return ""

   where
      getPubYear' md = return . maybe "" ('_' :) . foldr mplus Nothing .
         map (\f -> f (metaDates md))

      pubAttrs =
         [ getDateWithAttr "original-publication"
         , getDateWithAttr "publication"
         ]


getDateWithAttr :: String -> [Date] -> Maybe String
getDateWithAttr attrVal mds = foldr mplus Nothing $ map getPublication' mds
   where
      getPublication' (Date (Just av) d)
         | av == attrVal = extractYear d
      getPublication' _  = Nothing


getFirstDate :: [Date] -> Maybe String
getFirstDate ((Date _ d) : _) = extractYear d
getFirstDate _                    = Nothing


extractYear :: String -> Maybe String
extractYear s = case matchRegex (mkRegex "(^| )([0-9]{4})") s of
   Just (_ : y : []) -> Just y
   _                 -> Nothing
