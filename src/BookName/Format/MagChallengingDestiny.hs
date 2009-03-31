{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.MagChallengingDestiny
   ( fmtMagChallengingDestiny )
   where

import Control.Monad.Error
import Text.Printf

import BookName.Format.Util ( filterCommon, format )
import BookName.Util ( Fields )


fmtMagChallengingDestiny :: (MonadError String m) =>
   Fields -> m (String, String)
fmtMagChallengingDestiny = format "MagChallengingDestiny"
   "Crystalline Sphere Authors.*" (const "")
   "(Challenging Destiny) #([0-9]+).*" title


title :: String -> [String] -> String
title _ (name:issue:_) =
   printf "%sMagazine%03d" (filterCommon name) (read issue :: Int)
title _ _              = undefined
