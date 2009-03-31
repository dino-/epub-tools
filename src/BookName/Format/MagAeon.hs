{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.MagAeon
   ( fmtMagAeon )
   where

import Control.Monad.Error

import BookName.Format.Util ( format )
import BookName.Util ( Fields )


fmtMagAeon :: (MonadError String m) => Fields -> m (String, String)
fmtMagAeon = format "MagAeon"
   ".* Authors" (const "")
   "^A[eE]on ([^ ]+)$" titleMagAeon


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
titleMagAeon _ _      = undefined
