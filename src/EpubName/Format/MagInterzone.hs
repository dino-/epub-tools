-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

module EpubName.Format.MagInterzone
   ( fmtMagInterzone )
   where

import Codec.Epub.Opf.Package.Metadata
import Control.Monad.Error
import Text.Printf

import EpubName.Format.Util ( format )


fmtMagInterzone :: (MonadError String m) => Metadata -> m (String, String)
fmtMagInterzone = format "MagInterzone"
   ".* Authors" (const "")
   "^(Interzone)[^0-9]*([0-9]+)$" title


title :: String -> [String] -> String
title _ (name:issue:_) =
   printf "%sSFF%d" name (read issue :: Int)
title _ _              = undefined
