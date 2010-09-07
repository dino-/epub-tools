{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.MagLightspeed
   ( fmtMagLightspeed )
   where

import Control.Monad.Error
import Text.Printf

import BookName.Format.Util ( format, monthNum )
import BookName.Util ( Fields )


fmtMagLightspeed :: (MonadError String m) => Fields -> m (String, String)
fmtMagLightspeed = format "MagInterzone"
   ".*Adams.*" (const "")
   "(Lightspeed) .*, (.*) (.*)" title


title :: String -> [String] -> String
title _ (name:month:year:_) =
   printf "%s%s-%s" name year (monthNum month)
title _ _              = undefined
