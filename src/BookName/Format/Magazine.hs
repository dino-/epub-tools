{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.Magazine
   ( formatMagazine )
   where

import Control.Monad.Error
import Data.List hiding ( lookup )
import Data.Map hiding ( map )
import Prelude hiding ( lookup )

import BookName ( Fields, commonFilters, extractYear, 
   formatAuthor, formatTitle, lookupE )


formatMagazine :: (MonadError String m) => Fields -> m String
formatMagazine fs = do
   newAuthor <- lookupE "Author" fs >>= formatAuthor authorPatterns
   let year = extractYear $ lookup "FreeText" fs
   newTitle <- lookupE "Title" fs >>= formatTitle titlePatterns year
   return $ newAuthor ++ newTitle ++ ".lrf"


authorPatterns :: [(String, [String] -> String)]
authorPatterns =
   [ ( "Dell Magazine.*", const "" )
   , ( ".* Authors", const "" )
   , ( "Spilogale.*", const "" )
   , ( "Vander Neut Publications.*", const "" )
   , ( "Crystalline Sphere Publishing.*", const "" )
   ]


titleMagAeon :: String -> [String] -> String
titleMagAeon _ (numWord:_) = "AeonMagazine" ++ (num numWord)
   where
      num "One"       = "01"
      num "Two"       = "02"
      num "Three"     = "03"
      num "Four"      = "04"
      num "Five"      = "05"
      num "Six"       = "06"
      num "Seven"     = "07"
      num "Eight"     = "08"
      num "Nine"      = "09"
      num "Ten"       = "10"
      num "Eleven"    = "11"
      num "Twelve"    = "12"
      num "Thirteen"  = "13"
      num "Fourteen"  = "14"
      num "Fifteen"   = "15"
      num "Sixteen"   = "16"
      num "Seventeen" = "17"
      num "Eighteen"  = "18"
      num "Nineteen"  = "19"
      num "Twenty"    = "20"
      num x           = "[ERROR titleMagAeon " ++ x ++ "]"
titleMagAeon _ _ = undefined


titleFsfMag :: String -> [String] -> String
titleFsfMag _ (prefix:rest) =
   titleMagYM "foo" ((prefix ++ "Magazine"):rest)
titleFsfMag _ _ = undefined


titleMagYM :: String -> [String] -> String
titleMagYM _ (prefix:month:year:_) =
   prefix' ++ year ++ "-" ++ (monthNum month)
   where
      prefix' = foldl (flip id) prefix commonFilters
      monthNum "January"            = "01"
      monthNum "January-February"   = "01_02"
      monthNum "February"           = "02"
      monthNum "March"              = "03"
      monthNum "April"              = "04"
      monthNum "April-May"          = "04_05"
      monthNum "May"                = "05"
      monthNum "June"               = "06"
      monthNum "July"               = "07"
      monthNum "July-August"        = "07_08"
      monthNum "Jul-Aug"            = "07_08"
      monthNum "August"             = "08"
      monthNum x
         | isPrefixOf x "September" = "09"
      monthNum "October"            = "10"
      monthNum "October-November"   = "10_11"
      monthNum "November"           = "11"
      monthNum "December"           = "12"
      monthNum x                    = "[ERROR titleMagYM " ++ x ++ "]"
titleMagYM _ _ = undefined


titleMagInterzone :: String -> [String] -> String
titleMagInterzone _ (prefix:num:_) = prefix ++ "SFFMagazine" ++ num
titleMagInterzone _ _ = undefined


titlePatterns :: [(String, String -> [String] -> String)]
titlePatterns =
   [ ( "^(FSF).* ([^ ]+) ([0-9]{4})$", titleFsfMag )
   , ( "^A[eE]on ([^ ]+)$", titleMagAeon )
   , ( "^(Interzone)[^0-9]*([0-9]+)$", titleMagInterzone )
   , ( "(.*) ([^ ]+) ([0-9]{4})$", titleMagYM )
   ]
