{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.MagDell
   ( fmtMagDell )
   where

import Codec.Epub.Opf.Package.Metadata
import Control.Monad.Error
import Text.Printf

import BookName.Format.Util ( filterCommon, format, monthNum )


fmtMagDell :: (MonadError String m) => Metadata -> m (String, String)
fmtMagDell = format "MagDell"
   "Dell Magazine.*" (const "")
   "([^ ]*).*, ([^ ]+) ([0-9]{4})$" title


title :: String -> [String] -> String
title _ (prefix:month:year:_) =
   printf "%sSF%s-%s" (filterCommon prefix) year (monthNum month)
title _ _ = undefined
