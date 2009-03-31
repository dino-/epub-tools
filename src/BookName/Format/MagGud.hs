{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.MagGud
   ( fmtMagGud )
   where

import Control.Monad.Error
import Text.Printf

import BookName.Format.Util ( format )
import BookName.Util ( Fields )


fmtMagGud :: (MonadError String m) => Fields -> m (String, String)
fmtMagGud = format "MagGud"
   ".* Authors" (const "")
   "GUD Magazine Issue ([0-9]+) ::.*" title


title :: String -> [String] -> String
title _ (issue:_) =
   printf "GUDMagazine%02d" (read issue :: Int)
title _ _              = undefined
