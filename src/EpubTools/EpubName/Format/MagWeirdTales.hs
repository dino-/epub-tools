-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubName.Format.MagWeirdTales
   ( fmtMagWeirdTales )
   where

import Codec.Epub.Opf.Package.Metadata
import Text.Printf

import EpubTools.EpubName.Format.Util ( filterCommon, format )
import EpubTools.EpubName.Util


fmtMagWeirdTales :: Metadata -> EN (String, String)
fmtMagWeirdTales = format "MagWeirdTales"
   ".*" (const "")
   "^(Weird Tales)[^0-9]*([0-9]+)$" title


title :: String -> [String] -> String
title _ (name:issue:_) =
   printf "%s%03d" (filterCommon name) (read issue :: Int)
title _ _              = undefined
