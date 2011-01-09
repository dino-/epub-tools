-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

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
