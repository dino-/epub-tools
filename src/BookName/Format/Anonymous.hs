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


fmtAnonymous :: (MonadError String m) => Fields -> m String
fmtAnonymous = format
   "(.*)(Anonymous)" authorSingle
   "(.*)" titleSimple
