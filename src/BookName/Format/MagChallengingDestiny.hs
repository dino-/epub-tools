{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.MagChallengingDestiny
   ( fmtMagChallengingDestiny )
   where

import Codec.Epub.Opf.Package.Metadata
import Control.Monad.Error
import Text.Printf

import BookName.Format.Util ( filterCommon, format )


fmtMagChallengingDestiny :: (MonadError String m) =>
   Metadata -> m (String, String)
fmtMagChallengingDestiny = format "MagChallengingDestiny"
   "Crystalline Sphere .*" (const "")
   "(Challenging Destiny) #([0-9]+).*" title


title :: String -> [String] -> String
title _ (name:issue:_) =
   printf "%sMagazine%03d" (filterCommon name) (read issue :: Int)
title _ _              = undefined
