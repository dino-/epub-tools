-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubName.Format.MagGud
   ( fmtMagGud )
   where

import Codec.Epub.Opf.Package.Metadata
import Text.Printf

import EpubName.Format.Util ( format )
import EpubName.Util


fmtMagGud :: Metadata -> EN (String, String)
fmtMagGud = format "MagGud"
   "GUD Magazine Authors" (const "")
   ".* Magazine Issue ([0-9]+) ::.*" title


title :: String -> [String] -> String
title _ (issue:_) =
   printf "GUDMagazine%02d" (read issue :: Int)
title _ _              = undefined
