-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

module EpubName.Format.MagNemesis
   ( fmtMagNemesis )
   where

import Codec.Epub.Opf.Package.Metadata
import Control.Monad.Error
import Text.Printf

import EpubName.Format.Util ( filterCommon, format )


fmtMagNemesis :: (MonadError String m) => Metadata -> m (String, String)
fmtMagNemesis = format "MagNemesis"
   "Stephen Adams" (const "")
   "(Nemesis Mag)azine #([0-9]+).*" title


title :: String -> [String] -> String
title _ (name:issue:_) =
   printf "%s%03d" (filterCommon name) (read issue :: Int)
title _ _              = undefined
