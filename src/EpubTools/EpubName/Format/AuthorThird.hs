-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

module EpubTools.EpubName.Format.AuthorThird
   ( fmtAuthorThird )
   where

import Control.Monad.Error

import EpubTools.EpubName.Format.Util
   ( format
   , authorPostfix, titleSimple
   )
import EpubTools.EpubName.Util ( Fields )


fmtAuthorThird :: (MonadError String m) => Fields -> m (String, String)
fmtAuthorThird = format "AuthorThird"
   "(.*) ([^ ]+) (III)$" authorPostfix
   "(.*)" titleSimple
