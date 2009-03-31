{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.MagNemesis
   ( fmtMagNemesis )
   where

import Control.Monad.Error
import Text.Printf

import BookName.Format.Util ( filterCommon, format )
import BookName.Util ( Fields )


fmtMagNemesis :: (MonadError String m) => Fields -> m (String, String)
fmtMagNemesis = format "MagNemesis"
   "Stephen Adams" (const "")
   "(Nemesis Mag)azine #([0-9]+).*" title


title :: String -> [String] -> String
title _ (name:issue:_) =
   printf "%s%03d" (filterCommon name) (read issue :: Int)
title _ _              = undefined
