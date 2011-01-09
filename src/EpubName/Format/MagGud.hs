-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

module EpubName.Format.MagGud
   ( fmtMagGud )
   where

import Codec.Epub.Opf.Package.Metadata
import Control.Monad.Error
import Text.Printf

import EpubName.Format.Util ( format )


fmtMagGud :: (MonadError String m) => Metadata -> m (String, String)
fmtMagGud = format "MagGud"
   "GUD Magazine Authors" (const "")
   ".* Magazine Issue ([0-9]+) ::.*" title


title :: String -> [String] -> String
title _ (issue:_) =
   printf "GUDMagazine%02d" (read issue :: Int)
title _ _              = undefined