-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubName.Format
   ( tryFormatting
   )
   where

import Codec.Epub.Opf.Package
import Control.Monad.Error
import Data.List ( isPrefixOf )
import Text.Printf
import Text.Regex

import EpubTools.EpubName.Author
import EpubTools.EpubName.Opts
import EpubTools.EpubName.Util


formatters :: [Metadata -> EN (String, String)]
formatters =
   [ magAeon
   , magAnalog
   , magApex
   , magBcs
   , magBlackStatic
   , magChallengingDestiny
   , magClarkesworld
   , magEclipse
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
   , sfBestOfYear
   , book
   ]


magAeon :: Metadata -> EN (String, String)
magAeon md = do
   (issue:_) <- extractTitle md "^A[eE]on ([^ ]+)$"
   let title = printf "AeonMagazine%s" (wordNum issue)

   finish md "magAeon" [title]


magAnalog :: Metadata -> EN (String, String)
magAnalog md = do
   (prefix:month:year:_) <-
      extractTitle md "^(A[^ ]*).*, ([^ ]+) ([0-9]{4})$"

   let title = printf "%sSF%s-%s"
         (filterCommon prefix) year (monthNum month)

   finish md "magAnalog" [title]


magApex :: Metadata -> EN (String, String)
magApex md = do
   (prefix:issue:_) <- extractTitle md "^(Apex)[^0-9]*([0-9]+)$"
   let title = printf "%sMagazine%03d"
         (filterCommon prefix) (read issue :: Int)

   finish md "magApex" [title]


magBcs :: Metadata -> EN (String, String)
magBcs md = do
   (prefix:issue:_) <-
      extractTitle md "(Beneath Ceaseless.*) #([0-9]+).*"
   let title = printf "%s_Issue%03d"
         (filterCommon prefix) (read issue :: Int)

   finish md "magBcs" [title]


magBlackStatic :: Metadata -> EN (String, String)
magBlackStatic md = do
   (prefix:issue:_) <-
      extractTitle md "^(Black Static Horror Magazine)[^0-9]*([0-9]+)$"
   let title = printf "%s%02d" (filterCommon prefix) (read issue :: Int)

   finish md "magBlackStatic" [title]


magChallengingDestiny :: Metadata -> EN (String, String)
magChallengingDestiny md = do
   (prefix:issue:_) <- extractTitle md "^(Challenging Destiny) #([0-9]+).*"
   let title = printf "%sMagazine%03d"
         (filterCommon prefix) (read issue :: Int)

   finish md "magChallengingDestiny" [title]


magClarkesworld :: Metadata -> EN (String, String)
magClarkesworld md = do
   (prefix:issue:_) <-
      extractTitle md "^(Clarkesworld)[^0-9]*([0-9]+)$"
   let title = printf "%s%03d" prefix (read issue :: Int)

   finish md "magClarkesworld" [title]


magEclipse :: Metadata -> EN (String, String)
magEclipse md = do
   (prefix:issue:_) <-
      extractTitle md "^(Eclipse) ([^ ]+)$"
   let title = printf "%s%s" prefix (wordNum issue)

   finish md "magEclipse" [title]


magFsf :: Metadata -> EN (String, String)
magFsf md = do
   (month:year:_) <- extractTitle md "^FSF.* ([^ ]+) ([0-9]+)$"
   let title = printf "FantasyScienceFiction%s-%s"
         year (monthNum month)

   finish md "magFsf" [title]


magFutureOrbits :: Metadata -> EN (String, String)
magFutureOrbits md = do
   (prefix:issue:month:year:_) <- extractTitle md
      "(Future Orbits) Issue ([0-9]+), ([^ ]+) ([0-9]{4})$"
   let title = printf "%sMagazine%02d_%s-%s" (filterCommon prefix)
         (read issue :: Int) year (monthNum month)

   finish md "magFutureOrbits" [title]


magGud :: Metadata -> EN (String, String)
magGud md = do
   (issue:_) <-
      extractTitle md "GUD Magazine Issue ([0-9]+).*"
   let title = printf "GUDMagazine%02d" (read issue :: Int)

   finish md "magGud" [title]


magInterzone :: Metadata -> EN (String, String)
magInterzone md = do
   (prefix:issue:_) <-
      extractTitle md "^(Interzone)[^0-9]*([0-9]+)$"
   let title = printf "%sSFF%03d" prefix (read issue :: Int)

   finish md "magInterzone" [title]


magLightspeedDate :: Metadata -> EN (String, String)
magLightspeedDate md = do
   (prefix:month:year:_) <-
      extractTitle md "(Lightspeed) Magazine, (.*) (.*)"
   let title = printf "%s%s-%s"
         prefix year (monthNum month)

   finish md "magLightspeedDate" [title]


magLightspeedIssue :: Metadata -> EN (String, String)
magLightspeedIssue md = do
   (prefix:issue:_) <-
      extractTitle md "(Lightspeed) Magazine Issue (.*)"
   let title = printf "%s%03d" prefix (read issue :: Int)

   finish md "magLightspeedIssue" [title]


magNemesis :: Metadata -> EN (String, String)
magNemesis md = do
   (prefix:issue:_) <-
      extractTitle md "(Nemesis Mag)azine #([0-9]+).*"
   let title = printf "%s%03d" (filterCommon prefix) (read issue :: Int)

   finish md "magNemesis" [title]


magRageMachine :: Metadata -> EN (String, String)
magRageMachine md = do
   (prefix:month:year:_) <-
      extractTitle md "(Rage Machine.*)--([^ ]+) ([0-9]{4})$"
   let title = printf "%s_%s-%s"
         (filterCommon prefix) year (monthNum month)

   finish md "magRageMachine" [title]


magSomethingWicked :: Metadata -> EN (String, String)
magSomethingWicked md = do
   (prefix:issue:_) <- extractTitle md "^(Something Wicked)[^0-9]*([0-9]+)$"
   let title = printf "%sMagazine%02d"
         (filterCommon prefix) (read issue :: Int)

   finish md "magSomethingWicked" [title]


magUniverse :: Metadata -> EN (String, String)
magUniverse md = do
   (prefix:vol:num:_) <-
      extractTitle md "^(Jim Baen's Universe)-Vol ([^ ]+) Num ([^ ]+)"
   let title = printf "%sVol%02dNum%02d" (filterCommon prefix)
         (read vol :: Int) (read num :: Int)

   finish md "magUniverse" [title]


magWeirdTales :: Metadata -> EN (String, String)
magWeirdTales md = do
   (prefix:issue:_) <- extractTitle md "^(Weird Tales)[^0-9]*([0-9]+)$"
   let title = printf "%s%03d"
         (filterCommon prefix) (read issue :: Int)

   finish md "magWeirdTales" [title]


sfBestOfYear :: Metadata -> EN (String, String)
sfBestOfYear md = do
   (title:_) <- extractTitle md "(.*The Best of the Year.*)"

   finish md "sfBestOfYear" [filterCommon title]


book :: Metadata -> EN (String, String)
book md = do
   (title:_) <- extractTitle md "(.*)"
   year <- extractYear md

   finish md "book" [extractAuthors md, filterCommon title, year]


tryFormatting :: FilePath -> Metadata -> EN (String, String)
tryFormatting oldPath md = do
   foldr mplus
      (throwError $ printf "%s [ERROR No formatter found]" oldPath) $
      map (\f -> f md) formatters


finish :: Metadata -> String -> [String] -> EN (String, String)
finish md label parts = do
   publisher <- fmap (extractPublisher md) $ asks optPublisher
   return ( label
      , foldr1 (++) (parts ++ [publisher, ".epub"]))


extractTitle :: Metadata -> String -> EN [String]
extractTitle md re = do
   (MetaTitle _ oldTitle) <- case metaTitles md of
      [] -> throwError "format failed, no title present"
      ts -> return . head $ ts

   case matchRegex (mkRegex re) oldTitle of
      Just matches -> return matches
      Nothing      -> throwError $ printf "extract title failed: %s" re


{- Look for a date tag with event="original-publication" in the
   metadata
-}
extractYear :: Metadata -> EN String
extractYear md = do
   inclY <- asks optYear
   return . maybe "" ('_' :) $
      (foldr mplus Nothing (map (maybeYear inclY) $ metaDates md))

   where
      maybeYear True (MetaDate (Just "original-publication") d) = Just d
      maybeYear _    _                                          = Nothing


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
wordNum "One"       = "01"
wordNum "Two"       = "02"
wordNum "Three"     = "03"
wordNum "Four"      = "04"
wordNum "Five"      = "05"
wordNum "Six"       = "06"
wordNum "Seven"     = "07"
wordNum "Eight"     = "08"
wordNum "Nine"      = "09"
wordNum "Ten"       = "10"
wordNum "Eleven"    = "11"
wordNum "Twelve"    = "12"
wordNum "Thirteen"  = "13"
wordNum "Fourteen"  = "14"
wordNum "Fifteen"   = "15"
wordNum "Sixteen"   = "16"
wordNum "Seventeen" = "17"
wordNum "Eighteen"  = "18"
wordNum "Nineteen"  = "19"
wordNum "Twenty"    = "20"
wordNum x           = "[ERROR wordNum " ++ x ++ "]"
