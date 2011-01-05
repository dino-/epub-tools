{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.MagFsf
   ( fmtMagFsf )
   where

import Codec.Epub.Opf.Package.Metadata
import Control.Monad.Error
import Text.Printf

import BookName.Format.Util ( format, monthNum )


fmtMagFsf :: (MonadError String m) => Metadata -> m (String, String)
fmtMagFsf = format "MagFsf"
   "Spilogale.*" (const "")
   ".* ([^ ]+) ([0-9]{4})$" title


title :: String -> [String] -> String
title _ (month:year:_) =
   printf "FantasyScienceFiction%s-%s" year (monthNum month)
title _ _ = undefined
