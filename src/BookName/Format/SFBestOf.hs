{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.SFBestOf
   ( fmtSFBestOf )
   where

import Codec.Epub.Opf.Package.Metadata
import Control.Monad.Error

import BookName.Format.Util ( filterCommon, format )


fmtSFBestOf :: (MonadError String m) => Metadata -> m (String, String)
fmtSFBestOf = format "SFBestOf"
   "Rich Horton.*" (const "")
   "(.*)" title


title :: String -> [String] -> String
title _ (name:_) = filterCommon name
title _ _ = undefined
