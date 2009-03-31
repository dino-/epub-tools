{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.MagDell
   ( fmtMagDell )
   where

import Control.Monad.Error
import Text.Printf

import BookName.Format.Util ( filterCommon, format, monthNum )
import BookName.Util ( Fields )


fmtMagDell :: (MonadError String m) => Fields -> m (String, String)
fmtMagDell = format "MagDell"
   "Dell Magazine.*" (const "")
   "(.*) ([^ ]+) ([0-9]{4})$" title


title :: String -> [String] -> String
title _ (prefix:month:year:_) =
   printf "%s%s-%s" (filterCommon prefix) year (monthNum month)
title _ _ = undefined
