-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.MagLightspeed
   ( fmtMagLightspeed )
   where

import Codec.Epub.Opf.Package.Metadata
import Control.Monad.Error
import Text.Printf

import BookName.Format.Util ( format, monthNum )


fmtMagLightspeed :: (MonadError String m) => Metadata -> m (String, String)
fmtMagLightspeed = format "MagInterzone"
   ".*Adams.*" (const "")
   "(Lightspeed) .*, (.*) (.*)" title


title :: String -> [String] -> String
title _ (name:month:year:_) =
   printf "%s%s-%s" name year (monthNum month)
title _ _              = undefined
