-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubName.Format.MagLightspeed
   ( fmtMagLightspeed )
   where

import Codec.Epub.Opf.Package.Metadata
import Text.Printf

import EpubTools.EpubName.Format.Util ( format, monthNum )
import EpubTools.EpubName.Util


fmtMagLightspeed :: Metadata -> EN (String, String)
fmtMagLightspeed = format "MagInterzone"
   ".*" (const "")
   "(Lightspeed) Magazine, (.*) (.*)" title


title :: String -> [String] -> String
title _ (name:month:year:_) =
   printf "%s%s-%s" name year (monthNum month)
title _ _              = undefined
