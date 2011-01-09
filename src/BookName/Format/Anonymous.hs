-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.Anonymous
   ( fmtAnonymous )
   where

import Control.Monad.Error

import BookName.Format.Util
   ( format
   , authorSingle , titleSimple
   )
import BookName.Util ( Fields )


fmtAnonymous :: (MonadError String m) => Fields -> m (String, String)
fmtAnonymous = format "Anonymous"
   "(.*)(Anonymous)" authorSingle
   "(.*)" titleSimple
