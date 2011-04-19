-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubName.Format.SFBestOf
   ( fmtSFBestOf )
   where

import Codec.Epub.Opf.Package.Metadata
import Text.Printf

import EpubTools.EpubName.Format.Util ( filterCommon, format )
import EpubTools.EpubName.Util


fmtSFBestOf :: Metadata -> EN (String, String)
fmtSFBestOf = format "SFBestOf"
   "Rich Horton.*" (const "")
   "(.*)" title


title :: String -> [String] -> String
title year (name:_) = printf "%s%s" (filterCommon name) year
title _ _ = undefined