{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.AuthorThird
   ( fmtAuthorThird )
   where

import Control.Monad.Error

import BookName.Format.Util
   ( format
   , authorPostfix, titleSimple
   )
import BookName.Util ( Fields )


fmtAuthorThird :: (MonadError String m) => Fields -> m (String, String)
fmtAuthorThird = format "AuthorThird"
   "(.*) ([^ ]+) (III)$" authorPostfix
   "(.*)" titleSimple
