{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.MagSomethingWicked
   ( fmtMagSomethingWicked )
   where

import Control.Monad.Error
import Text.Printf

import BookName.Format.Util ( filterCommon, format )
import BookName.Util ( Fields )


fmtMagSomethingWicked :: (MonadError String m) => Fields -> m (String, String)
fmtMagSomethingWicked = format "MagSomethingWicked"
   ".* Authors" (const "")
   "(Something Wicked).* #([0-9]+).*" title


title :: String -> [String] -> String
title _ (name:issue:_) =
   printf "%sMagazine%02d" (filterCommon name) (read issue :: Int)
title _ _              = undefined
