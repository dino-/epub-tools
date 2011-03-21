-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubName.Format.MagUniverse
   ( fmtMagUniverse )
   where

import Codec.Epub.Opf.Package.Metadata
import Text.Printf

import EpubTools.EpubName.Format.Util ( filterCommon, format )
import EpubTools.EpubName.Util


fmtMagUniverse :: Metadata -> EN (String, String)
fmtMagUniverse = format "MagUniverse"
   ".*" (const "")
   "^(Jim Baen's Universe)-Vol ([^ ]+) Num ([^ ]+)" title


title :: String -> [String] -> String
title _ (name:vol:num:_) =
   printf "%sVol%02dNum%02d" (filterCommon name) (read vol :: Int) 
      (read num :: Int)
title _ _         = undefined
