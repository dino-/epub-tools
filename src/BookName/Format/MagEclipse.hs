{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.MagEclipse
   ( fmtMagEclipse )
   where

import Codec.Epub.Opf.Package.Metadata
import Control.Monad.Error

import BookName.Format.Util ( format )


fmtMagEclipse :: (MonadError String m) => Metadata -> m (String, String)
fmtMagEclipse = format "MagAeon"
   ".*" (const "")
   "^(Eclipse) ([^ ]+)$" titleMagEclipse


titleMagEclipse :: String -> [String] -> String
titleMagEclipse _ (magName:numWord:_) = magName ++ (num numWord)
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
      num x           = "[ERROR titleMagEclipse " ++ x ++ "]"
titleMagEclipse _ _      = undefined
