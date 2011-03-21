-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubName.Format.MagRageMachine
   ( fmtMagRageMachine )
   where

import Codec.Epub.Opf.Package.Metadata
import Text.Printf

import EpubTools.EpubName.Format.Util ( filterCommon, format, monthNum )
import EpubTools.EpubName.Util


fmtMagRageMachine :: Metadata -> EN (String, String)
fmtMagRageMachine = format "MagRageMachine"
   ".*" (const "")
   "(Rage Machine.*)--([^ ]+) ([0-9]{4})$" title


title :: String -> [String] -> String
title _ (name:month:year:_) =
   printf "%s_%s-%s" (filterCommon name) year (monthNum month)
title _ _ = undefined
