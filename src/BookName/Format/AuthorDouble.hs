{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.AuthorDouble
   ( fmtAuthorDouble )
   where

import Control.Monad.Error
import Prelude hiding ( last )
import Text.Printf

import BookName.Format.Util ( filterCommon, format, titleSimple )
import BookName.Util ( Fields )


fmtAuthorDouble :: (MonadError String m) => Fields -> m (String, String)
fmtAuthorDouble = format "AuthorDouble"
   ".* ([^ ]+) & .* ([^ ]+)" authorDouble
   "(.*)" titleSimple


authorDouble :: [String] -> String
authorDouble (last1:last2:_) = 
   printf "%s_%s-" (filterCommon last1) (filterCommon last2)
authorDouble _               = undefined
