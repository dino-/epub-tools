-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubName.Format.MagAnalog
   ( fmtMagAnalog )
   where

import Codec.Epub.Opf.Package.Metadata
import Text.Printf

import EpubTools.EpubName.Format.Util ( filterCommon, format, monthNum )
import EpubTools.EpubName.Util


fmtMagAnalog :: Metadata -> EN (String, String)
fmtMagAnalog = format "MagAnalog"
   "Dell Magazine.*" (const "")
   "([^ ]*).*, ([^ ]+) ([0-9]{4})$" title


title :: String -> [String] -> String
title _ (prefix:month:year:_) =
   printf "%sSF%s-%s" (filterCommon prefix) year (monthNum month)
title _ _ = undefined
