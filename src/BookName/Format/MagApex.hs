{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.MagApex
   ( fmtMagApex )
   where

import Control.Monad.Error
import Text.Printf

import BookName.Format.Util ( filterCommon, format )
import BookName.Util ( Fields )


fmtMagApex :: (MonadError String m) => Fields -> m String
fmtMagApex = format
   ".* Authors" (const "")
   "(Apex.*) #([0-9]+).*" title


title :: String -> [String] -> String
title _ (name:issue:_) =
   printf "%s%02d" (filterCommon name) (read issue :: Int)
title _ _              = undefined
