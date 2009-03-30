{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.MagNemesis
   ( fmtMagNemesis )
   where

import Control.Monad.Error
import Text.Printf

import BookName.Format.Util ( commonFilters, format )
import BookName.Util ( Fields )


fmtMagNemesis :: (MonadError String m) => Fields -> m String
fmtMagNemesis = format "Stephen Adams" (const "") "(Nemesis Mag)azine #([0-9]+).*" title


title :: String -> [String] -> String
title _ (name:issue:_) = printf "%s%03d" nameCleaned (read issue :: Int)
   where
      nameCleaned = foldl (flip id) name commonFilters
title _ _              = undefined
