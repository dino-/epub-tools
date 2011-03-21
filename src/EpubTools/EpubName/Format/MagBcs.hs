-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubName.Format.MagBcs
   ( fmtMagBcs )
   where

import Codec.Epub.Opf.Package.Metadata
import Text.Printf

import EpubTools.EpubName.Format.Util ( filterCommon, format )
import EpubTools.EpubName.Util


fmtMagBcs :: Metadata -> EN (String, String)
fmtMagBcs = format "MagBcs"
   ".*" (const "")
   "(Beneath Ceaseless.*) #([0-9]+).*" title


title :: String -> [String] -> String
title _ (name:issue:_) =
   printf "%s_Issue%03d" (filterCommon name) (read issue :: Int)
title _ _              = undefined
