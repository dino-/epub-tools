{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.MagFsf
   ( fmtMagFsf )
   where

import Control.Monad.Error
import Text.Printf

import BookName.Format.Util ( format, monthNum )
import BookName.Util ( Fields )


fmtMagFsf :: (MonadError String m) => Fields -> m (String, String)
fmtMagFsf = format "MagFsf"
   "Spilogale.*" (const "")
   ".* ([^ ]+) ([0-9]{4})$" title


title :: String -> [String] -> String
title _ (month:year:_) =
   printf "FantasyScienceFiction%s-%s" year (monthNum month)
title _ _ = undefined
