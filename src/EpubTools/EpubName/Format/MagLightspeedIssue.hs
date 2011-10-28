-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubName.Format.MagLightspeedIssue
   ( fmtMagLightspeedIssue )
   where

import Codec.Epub.Opf.Package.Metadata
import Text.Printf

import EpubTools.EpubName.Format.Util ( format )
import EpubTools.EpubName.Util


fmtMagLightspeedIssue :: Metadata -> EN (String, String)
fmtMagLightspeedIssue = format "MagLightspeedIssue"
   ".*" (const "")
   "(Lightspeed) Magazine Issue (.*)" title


title :: String -> [String] -> String
title _ (name:issue:_) =
   printf "%s%03d" name ((read issue) :: Int)
title _ _              = undefined
