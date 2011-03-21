-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubName.Format.MagFsf
   ( fmtMagFsf )
   where

import Codec.Epub.Opf.Package.Metadata
import Text.Printf

import EpubTools.EpubName.Format.Util ( format, monthNum )
import EpubTools.EpubName.Util


fmtMagFsf :: Metadata -> EN (String, String)
fmtMagFsf = format "MagFsf"
   "Spilogale.*" (const "")
   ".* ([^ ]+) ([0-9]{4})$" title


title :: String -> [String] -> String
title _ (month:year:_) =
   printf "FantasyScienceFiction%s-%s" year (monthNum month)
title _ _ = undefined
