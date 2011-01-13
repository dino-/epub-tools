-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubName.Format.MagApex
   ( fmtMagApex )
   where

import Codec.Epub.Opf.Package.Metadata
import Text.Printf

import EpubName.Format.Util ( format )
import EpubName.Util


fmtMagApex :: Metadata -> EN (String, String)
fmtMagApex = format "MagApex"
   ".*" (const "")
   "^Apex[^0-9]*([0-9]+)$" title


title :: String -> [String] -> String
title _ (issue:_) =
   printf "ApexMagazine%03d" (read issue :: Int)
title _ _         = undefined
