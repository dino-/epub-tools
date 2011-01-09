-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.MagAnalog
   ( fmtMagAnalog )
   where

import Codec.Epub.Opf.Package.Metadata
import Control.Monad.Error
import Text.Printf

import BookName.Format.Util ( filterCommon, format, monthNum )


fmtMagAnalog :: (MonadError String m) => Metadata -> m (String, String)
fmtMagAnalog = format "MagAnalog"
   "Dell Magazine.*" (const "")
   "([^ ]*).*, ([^ ]+) ([0-9]{4})$" title


title :: String -> [String] -> String
title _ (prefix:month:year:_) =
   printf "%sSF%s-%s" (filterCommon prefix) year (monthNum month)
title _ _ = undefined
