{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.MagGud
   ( fmtMagGud )
   where

import Codec.Epub.Opf.Package.Metadata
import Control.Monad.Error
import Text.Printf

import BookName.Format.Util ( format )


fmtMagGud :: (MonadError String m) => Metadata -> m (String, String)
fmtMagGud = format "MagGud"
   "GUD Magazine Authors" (const "")
   ".* Magazine Issue ([0-9]+) ::.*" title


title :: String -> [String] -> String
title _ (issue:_) =
   printf "GUDMagazine%02d" (read issue :: Int)
title _ _              = undefined
