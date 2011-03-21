-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubName.Format.MagSomethingWicked
   ( fmtMagSomethingWicked )
   where

import Codec.Epub.Opf.Package.Metadata
import Text.Printf

import EpubTools.EpubName.Format.Util ( filterCommon, format )
import EpubTools.EpubName.Util


fmtMagSomethingWicked :: Metadata -> EN (String, String)
fmtMagSomethingWicked = format "MagSomethingWicked"
   ".* Authors" (const "")
   "(Something Wicked).* #([0-9]+).*" title


title :: String -> [String] -> String
title _ (name:issue:_) =
   printf "%sMagazine%02d" (filterCommon name) (read issue :: Int)
title _ _              = undefined
