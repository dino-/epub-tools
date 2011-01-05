{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.MagNameIssue
   ( fmtMagNameIssue )
   where

import Codec.Epub.Opf.Package.Metadata
import Control.Monad.Error
import Text.Printf

import BookName.Format.Util ( filterCommon, format )


fmtMagNameIssue :: (MonadError String m) => Metadata -> m (String, String)
fmtMagNameIssue = format "MagNameIssue"
   ".* Authors" (const "")
   "(.*) #([0-9]+).*" title


title :: String -> [String] -> String
title _ (name:issue:_) =
   printf "%s%02d" (filterCommon name) (read issue :: Int)
title _ _              = undefined
