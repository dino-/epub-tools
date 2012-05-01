-- Copyright: 2008-2012 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubName.Format
   ( tryFormatting
   )
   where

import Codec.Epub.Opf.Package
import Control.Monad.Error
import Data.List ( isPrefixOf )
import Data.Maybe ( fromJust )
import Text.Printf
import Text.Regex

import EpubTools.EpubName.Author
import EpubTools.EpubName.Opts
import EpubTools.EpubName.PubYear
import EpubTools.EpubName.Util


formatters :: [EN (String, [String])]
formatters =
   [ magAeon
   , magAnalog
   , magApex
   , magBcs
   , magBlackStatic
   , magChallengingDestiny
   , magClarkesworld
   , magEclipse
   , magFantasyMag
   , magFsf
   , magFutureOrbits
   , magGud
   , magInterzone
   , magLightspeedDate
   , magLightspeedIssue
   , magNemesis
   , magRageMachine
   , magSomethingWicked
   , magWeirdTales
   , magUniverse
   , compOfTheYear
   , compYearsBest
   , book   -- Kind of always want this one last as a catch-all
   ]


magAeon :: EN (String, [String])
magAeon = do
   (issue:_) <- extractTitle "^A[eE]on ([^ ]+)$"
   let title = printf "AeonMagazine%s" (wordNum issue)

   return ("magAeon", [title])


magAnalog :: EN (String, [String])
magAnalog = do
   (prefix:month:year:_) <-
      extractTitle "^(A[^ ]*).*, ([^ ]+) ([0-9]{4})$"

   let title = printf "%sSF%s-%s"
         (filterCommon prefix) year (monthNum month)

   return ("magAnalog", [title])


magApex :: EN (String, [String])
magApex = do
   (prefix:issue:_) <- extractTitle "^(Apex)[^0-9]*([0-9]+)$"
   let title = printf "%sMagazine%03d"
         (filterCommon prefix) (read issue :: Int)

   return ("magApex", [title])


magBcs :: EN (String, [String])
magBcs = do
   (prefix:issue:_) <-
      extractTitle "(Beneath Ceaseless.*) #([0-9]+).*"
   let title = printf "%s_Issue%03d"
         (filterCommon prefix) (read issue :: Int)

   return ("magBcs", [title])


magBlackStatic :: EN (String, [String])
magBlackStatic = do
   (prefix:issue:_) <-
      extractTitle "^(Black Static Horror Magazine)[^0-9]*([0-9]+)$"
   let title = printf "%s%02d" (filterCommon prefix) (read issue :: Int)

   return ("magBlackStatic", [title])


magChallengingDestiny :: EN (String, [String])
magChallengingDestiny = do
   (prefix:issue:_) <- extractTitle "^(Challenging Destiny) #([0-9]+).*"
   let title = printf "%sMagazine%03d"
         (filterCommon prefix) (read issue :: Int)

   return ("magChallengingDestiny", [title])


magClarkesworld :: EN (String, [String])
magClarkesworld = do
   (prefix:issue:_) <-
      extractTitle "^(Clarkesworld)[^0-9]*([0-9]+)$"
   let title = printf "%s%03d" prefix (read issue :: Int)

   return ("magClarkesworld", [title])


magEclipse :: EN (String, [String])
magEclipse = do
   authorMatches "Jonathan Strahan"

   (prefix:issueRaw:_) <- extractTitle "^(Eclipse) ([^ :]+)"

   let issue = fromJust $
         mbWordNum issueRaw
         `mplus`
         (Just $ printf "%02d" (read issueRaw :: Int))

   let title = printf "%s%s" prefix issue

   return ("magEclipse", [title])


magFantasyMag :: EN (String, [String])
magFantasyMag = do
   (prefix:issue:_) <-
      extractTitle "^(Fantasy Magazine)[^0-9]+([0-9]+).*"
   let title = printf "%s%03d" (filterCommon prefix) (read issue :: Int)

   return ("magFantasyMag", [title])


magFsf :: EN (String, [String])
magFsf = do
   authorMatches "Spilogale"

   (month:year:_) <- extractTitle ".* ([^ ]+) ([0-9]{4})$"
   let title = printf "FantasyScienceFiction%s-%s"
         year (monthNum month)

   return ("magFsf", [title])


magFutureOrbits :: EN (String, [String])
magFutureOrbits = do
   (prefix:issue:month:year:_) <- extractTitle
      "(Future Orbits) Issue ([0-9]+), ([^ ]+) ([0-9]{4})$"
   let title = printf "%sMagazine%02d_%s-%s" (filterCommon prefix)
         (read issue :: Int) year (monthNum month)

   return ("magFutureOrbits", [title])


magGud :: EN (String, [String])
magGud = do
   authorMatches "GUD Magazine Authors"

   (issue:_) <-
      extractTitle ".* Magazine Issue ([0-9]+) ::.*"
   let title = printf "GUDMagazine%02d" (read issue :: Int)

   return ("magGud", [title])


magInterzone :: EN (String, [String])
magInterzone = do
   (prefix:issue:_) <-
      extractTitle "^(Interzone)[^0-9]*([0-9]+)$"
   let title = printf "%sSFF%03d" prefix (read issue :: Int)

   return ("magInterzone", [title])


magLightspeedDate :: EN (String, [String])
magLightspeedDate = do
   (prefix:month:year:_) <-
      extractTitle "(Lightspeed) Magazine, (.*) (.*)"
   let title = printf "%s%s-%s"
         prefix year (monthNum month)

   return ("magLightspeedDate", [title])


magLightspeedIssue :: EN (String, [String])
magLightspeedIssue = do
   (prefix:issue:_) <-
      extractTitle "(Lightspeed) Magazine Issue (.*)"
   let title = printf "%s%03d" prefix (read issue :: Int)

   return ("magLightspeedIssue", [title])


magNemesis :: EN (String, [String])
magNemesis = do
   (prefix:issue:_) <-
      extractTitle "(Nemesis Mag)azine #([0-9]+).*"
   let title = printf "%s%03d" (filterCommon prefix) (read issue :: Int)

   return ("magNemesis", [title])


magRageMachine :: EN (String, [String])
magRageMachine = do
   (prefix:month:year:_) <-
      extractTitle "(Rage Machine.*)--([^ ]+) ([0-9]{4})$"
   let title = printf "%s_%s-%s"
         (filterCommon prefix) year (monthNum month)

   return ("magRageMachine", [title])


magSomethingWicked :: EN (String, [String])
magSomethingWicked = do
   (prefix:issue:_) <- extractTitle "^(Something Wicked)[^0-9]*([0-9]+)"
   let title = printf "%s%03d"
         (filterCommon prefix) (read issue :: Int)

   return ("magSomethingWicked", [title])


magUniverse :: EN (String, [String])
magUniverse = do
   (prefix:vol:num:_) <-
      extractTitle "^(Jim Baen's Universe)-Vol ([^ ]+) Num ([^ ]+)"
   let title = printf "%sVol%02dNum%02d" (filterCommon prefix)
         (read vol :: Int) (read num :: Int)

   return ("magUniverse", [title])


magWeirdTales :: EN (String, [String])
magWeirdTales = do
   (prefix:issue:_) <- extractTitle "^(Weird Tales)[^0-9]*([0-9]+)$"
   let title = printf "%s%03d"
         (filterCommon prefix) (read issue :: Int)

   return ("magWeirdTales", [title])


compOfTheYear :: EN (String, [String])
compOfTheYear = do
   (title:_) <- extractTitle "(.*of the Year.*)"

   return ("compOfTheYear", [filterCommon title])


compYearsBest :: EN (String, [String])
compYearsBest = do
   (title:_) <- extractTitle "(.*Year's Best.*)"

   return ("compYearsBest", [filterCommon title])


book :: EN (String, [String])
book = do
   (title:_) <- extractTitle "(.*)"
   year <- getPubYear

   authors <- extractAuthors

   return ("book", [authors, filterCommon title, year])


tryFormatting :: FilePath -> EN (String, String)
tryFormatting oldPath = do
   foldr mplus
      (throwError $ printf "%s [ERROR No formatter found]" oldPath) $
      map (\f -> f >>= finish) formatters


finish :: (String, [String]) -> EN (String, String)
finish (label, parts) = do
   md <- asks gMetadata

   publisher <- fmap (extractPublisher md) $ asks $ optPublisher . gOpts
   return ( label
      , foldr1 (++) (parts ++ [publisher, ".epub"]))


extractTitle :: String -> EN [String]
extractTitle re = do
   md <- asks gMetadata

   (MetaTitle _ oldTitle) <- case metaTitles md of
      [] -> throwError "format failed, no title present"
      ts -> return . head $ ts

   case matchRegex (mkRegex re) oldTitle of
      Just matches -> return matches
      Nothing      -> throwError $ printf "extract title failed: %s" re


extractPublisher :: Metadata -> Bool -> String
extractPublisher _  False = ""
extractPublisher md True  = maybe "" ('_' :)
   (foldr mplus Nothing (map maybePub $ metaContributors md))

   where
      maybePub (MetaCreator (Just "bkp") (Just fa) _ ) = Just fa
      maybePub _                                       = Nothing


{- Convert an English month name (with creative ranges and variations)
   into number form
-}
monthNum :: String -> String
monthNum x
   | isPrefixOf x "January"   = "01"
monthNum "January-February"   = "01_02"
monthNum "January/February"   = "01_02"
monthNum "Jan-Feb"            = "01_02"
monthNum "Jan/Feb"            = "01_02"
monthNum x
   | isPrefixOf x "February"  = "02"
monthNum x
   | isPrefixOf x "March"     = "03"
monthNum "March-April"        = "03_04"
monthNum "March/April"        = "03_04"
monthNum "Mar-Apr"            = "03_04"
monthNum "Mar/Apr"            = "03_04"
monthNum x
   | isPrefixOf x "April"     = "04"
monthNum "April-May"          = "04_05"
monthNum "April/May"          = "04_05"
monthNum "Apr-May"            = "04_05"
monthNum "Apr/May"            = "04_05"
monthNum "May"                = "05"
monthNum "May-June"           = "05_06"
monthNum "May/June"           = "05_06"
monthNum "May-Jun"            = "05_06"
monthNum "May/Jun"            = "05_06"
monthNum x
   | isPrefixOf x "June"      = "06"
monthNum "June/July"          = "06_07"
monthNum "June-July"          = "06_07"
monthNum "Jun-Jul"            = "06_07"
monthNum "Jun/Jul"            = "06_07"
monthNum x
   | isPrefixOf x "July"      = "07"
monthNum "July-August"        = "07_08"
monthNum "July/August"        = "07_08"
monthNum "Jul-Aug"            = "07_08"
monthNum "Jul/Aug"            = "07_08"
monthNum x
   | isPrefixOf x "August"    = "08"
monthNum "August-September"   = "08_09"
monthNum "August/September"   = "08_09"
monthNum "Aug-Sep"            = "08_09"
monthNum "Aug/Sep"            = "08_09"
monthNum x
   | isPrefixOf x "September" = "09"
monthNum "September-October"  = "09_10"
monthNum x
   | isPrefixOf x "October"   = "10"
monthNum "October-November"   = "10_11"
monthNum "October/November"   = "10_11"
monthNum "Oct-Nov"            = "10_11"
monthNum "Oct/Nov"            = "10_11"
monthNum "November-December"  = "11_12"
monthNum x
   | isPrefixOf x "November"  = "11"
monthNum x
   | isPrefixOf x "December"  = "12"
monthNum x                    = "[ERROR monthNum " ++ x ++ "]"


{- Convert an English word for a number into number form
-}
wordNum :: String -> String
wordNum s = maybe ("[ERROR mbWordNum " ++ s ++ "]") id $ mbWordNum s

mbWordNum :: String -> Maybe String
mbWordNum "One"       = Just "01"
mbWordNum "Two"       = Just "02"
mbWordNum "Three"     = Just "03"
mbWordNum "Four"      = Just "04"
mbWordNum "Five"      = Just "05"
mbWordNum "Six"       = Just "06"
mbWordNum "Seven"     = Just "07"
mbWordNum "Eight"     = Just "08"
mbWordNum "Nine"      = Just "09"
mbWordNum "Ten"       = Just "10"
mbWordNum "Eleven"    = Just "11"
mbWordNum "Twelve"    = Just "12"
mbWordNum "Thirteen"  = Just "13"
mbWordNum "Fourteen"  = Just "14"
mbWordNum "Fifteen"   = Just "15"
mbWordNum "Sixteen"   = Just "16"
mbWordNum "Seventeen" = Just "17"
mbWordNum "Eighteen"  = Just "18"
mbWordNum "Nineteen"  = Just "19"
mbWordNum "Twenty"    = Just "20"
mbWordNum _           = Nothing
