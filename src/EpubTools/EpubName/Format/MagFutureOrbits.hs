-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubName.Format.MagFutureOrbits
   ( fmtMagFutureOrbits )
   where

import Codec.Epub.Opf.Package.Metadata
import Text.Printf

import EpubTools.EpubName.Format.Util ( filterCommon, format, monthNum )
import EpubTools.EpubName.Util


fmtMagFutureOrbits :: Metadata -> EN (String, String)
fmtMagFutureOrbits = format "MagFutureOrbits"
   ".*" (const "")
   "(Future Orbits) Issue ([0-9]+), ([^ ]+) ([0-9]{4})$" title


title :: String -> [String] -> String
title _ (name:issue:month:year:_) =
   printf "%sMagazine%02d_%s-%s"
      (filterCommon name) (read issue :: Int) year (monthNum month)
title _ _ = undefined
