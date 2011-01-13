-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubName.Format.MagInterzone
   ( fmtMagInterzone )
   where

import Codec.Epub.Opf.Package.Metadata
import Text.Printf

import EpubName.Format.Util ( format )
import EpubName.Util


fmtMagInterzone :: Metadata -> EN (String, String)
fmtMagInterzone = format "MagInterzone"
   ".* Authors" (const "")
   "^(Interzone)[^0-9]*([0-9]+)$" title


title :: String -> [String] -> String
title _ (name:issue:_) =
   printf "%sSFF%d" name (read issue :: Int)
title _ _              = undefined
