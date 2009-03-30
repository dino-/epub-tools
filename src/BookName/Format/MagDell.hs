{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.MagDell
   ( fmtMagDell )
   where

import Control.Monad.Error
import Data.List ( isPrefixOf )

import BookName.Format.Util ( filterCommon, format )
import BookName.Util ( Fields )


fmtMagDell :: (MonadError String m) => Fields -> m String
fmtMagDell = format "Dell Magazine.*" (const "") "(.*) ([^ ]+) ([0-9]{4})$" titleMagYM


-- FIXME This will be factored out later
{- Format a generic magazine title that contains month and year info
-}
titleMagYM :: String -> [String] -> String
titleMagYM _ (prefix:month:year:_) =
   prefix' ++ year ++ "-" ++ (monthNum month)
   where
      prefix' = filterCommon prefix
      monthNum "January"            = "01"
      monthNum "January-February"   = "01_02"
      monthNum "February"           = "02"
      monthNum "March"              = "03"
      monthNum "April"              = "04"
      monthNum "April-May"          = "04_05"
      monthNum "May"                = "05"
      monthNum "June"               = "06"
      monthNum "June/July"          = "06_07"
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
