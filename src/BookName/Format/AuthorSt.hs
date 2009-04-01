{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.AuthorSt
   ( fmtAuthorSt )
   where

import Control.Monad.Error

import BookName.Format.Util
   ( format
   , authorSingle, titleSimple
   )
import BookName.Util ( Fields )


fmtAuthorSt :: (MonadError String m) => Fields -> m (String, String)
fmtAuthorSt = format "AuthorSt"
   "(.*) (St\\. [^ ]+)$" authorSingle
   "(.*)" titleSimple
