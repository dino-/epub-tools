{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.MagFutureOrbits
   ( fmtMagFutureOrbits )
   where

import Codec.Epub.Opf.Package.Metadata
import Control.Monad.Error
import Text.Printf

import BookName.Format.Util ( filterCommon, format, monthNum )


fmtMagFutureOrbits :: (MonadError String m) => Metadata -> m (String, String)
fmtMagFutureOrbits = format "MagFutureOrbits"
   ".*" (const "")
   "(Future Orbits) Issue ([0-9]+), ([^ ]+) ([0-9]{4})$" title


title :: String -> [String] -> String
title _ (name:issue:month:year:_) =
   printf "%sMagazine%02d_%s-%s"
      (filterCommon name) (read issue :: Int) year (monthNum month)
title _ _ = undefined
