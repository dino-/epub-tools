{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.SFBestOf
   ( fmtSFBestOf )
   where

import Control.Monad.Error

import BookName.Format.Util ( filterCommon, format )
import BookName.Util ( Fields )


fmtSFBestOf :: (MonadError String m) => Fields -> m (String, String)
fmtSFBestOf = format "SFBestOf"
   "Rich Horton.*" (const "")
   "(.*)" title


title :: String -> [String] -> String
title _ (name:_) = filterCommon name
title _ _ = undefined
