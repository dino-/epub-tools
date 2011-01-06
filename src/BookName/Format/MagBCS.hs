{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.MagBCS
   ( fmtMagBCS )
   where

import Codec.Epub.Opf.Package.Metadata
import Control.Monad.Error
import Text.Printf

import BookName.Format.Util ( filterCommon, format )


fmtMagBCS :: (MonadError String m) =>
   Metadata -> m (String, String)
fmtMagBCS = format "MagBCS"
   ".*" (const "")
   "(Beneath Ceaseless.*) #([0-9]+).*" title


title :: String -> [String] -> String
title _ (name:issue:_) =
   printf "%s_Issue%03d" (filterCommon name) (read issue :: Int)
title _ _              = undefined
