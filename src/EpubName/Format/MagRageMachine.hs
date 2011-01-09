-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

module EpubName.Format.MagRageMachine
   ( fmtMagRageMachine )
   where

import Codec.Epub.Opf.Package.Metadata
import Control.Monad.Error
import Text.Printf

import EpubName.Format.Util ( filterCommon, format, monthNum )


fmtMagRageMachine :: (MonadError String m) => Metadata -> m (String, String)
fmtMagRageMachine = format "MagRageMachine"
   ".*" (const "")
   "(Rage Machine.*)--([^ ]+) ([0-9]{4})$" title


title :: String -> [String] -> String
title _ (name:month:year:_) =
   printf "%s_%s-%s" (filterCommon name) year (monthNum month)
title _ _ = undefined
