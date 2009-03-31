{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.MagInterzone
   ( fmtMagInterzone )
   where

import Control.Monad.Error
import Text.Printf

import BookName.Format.Util ( format )
import BookName.Util ( Fields )


fmtMagInterzone :: (MonadError String m) => Fields -> m (String, String)
fmtMagInterzone = format "MagInterzone"
   ".* Authors" (const "")
   "^(Interzone)[^0-9]*([0-9]+)$" title


title :: String -> [String] -> String
title _ (name:issue:_) =
   printf "%sSFF%d" name (read issue :: Int)
title _ _              = undefined
