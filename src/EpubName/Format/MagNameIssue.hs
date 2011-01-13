-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubName.Format.MagNameIssue
   ( fmtMagNameIssue )
   where

import Codec.Epub.Opf.Package.Metadata
import Text.Printf

import EpubName.Format.Util ( filterCommon, format )
import EpubName.Util


fmtMagNameIssue :: Metadata -> EN (String, String)
fmtMagNameIssue = format "MagNameIssue"
   ".* Authors" (const "")
   "(.*) #([0-9]+).*" title


title :: String -> [String] -> String
title _ (name:issue:_) =
   printf "%s%02d" (filterCommon name) (read issue :: Int)
title _ _              = undefined
