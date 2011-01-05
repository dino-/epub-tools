{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.MagSomethingWicked
   ( fmtMagSomethingWicked )
   where

import Codec.Epub.Opf.Package.Metadata
import Control.Monad.Error
import Text.Printf

import BookName.Format.Util ( filterCommon, format )


fmtMagSomethingWicked :: (MonadError String m) => Metadata -> m (String, String)
fmtMagSomethingWicked = format "MagSomethingWicked"
   ".* Authors" (const "")
   "(Something Wicked).* #([0-9]+).*" title


title :: String -> [String] -> String
title _ (name:issue:_) =
   printf "%sMagazine%02d" (filterCommon name) (read issue :: Int)
title _ _              = undefined
