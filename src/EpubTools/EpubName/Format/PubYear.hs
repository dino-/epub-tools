-- License: ISC (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubName.Format.PubYear
   ( extractYear
   , getPubYear
   )
   where

import Codec.Epub.Data.Metadata ( Date (..), DateEvent (Created, Epub,
  Issued, Modified), Metadata (metaDates) )
import Control.Monad ( (<=<) )
import qualified Data.Map.Strict as Map
import Data.Monoid ( First (..), getFirst )
import Text.Regex ( matchRegex, mkRegex )

import EpubTools.EpubName.Opts
import EpubTools.EpubName.Format.Util


{- Look for publication date, return "" if none found
-}
getPubYear :: EN String
getPubYear = do
  yearHandling <- asks $ optPubYear . gOpts
  let events = case yearHandling of
        AnyDate -> [Issued, Created, Epub, Modified]
        NoModified -> [Issued, Created, Epub]
        NoDate -> []
  datesMap <- asks (metaDates . gMetadata)
  let fs = map (flip Map.lookup $ datesMap) events
  pure . maybe "" ('_' :) . (extractYear <=< getFirst) . mconcat . map First $ fs


extractYear :: Date -> Maybe String
extractYear (Date dateString) =
  case matchRegex (mkRegex "(^| )([0-9]{4})") dateString of
    Just (_ : y : []) -> Just y
    _                 -> Nothing
