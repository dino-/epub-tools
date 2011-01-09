-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.AuthorDouble
   ( fmtAuthorDouble )
   where

import Codec.Epub.Opf.Package.Metadata
import Control.Monad.Error
import Data.Maybe ( fromJust )
import Prelude hiding ( last )
import Text.Printf
import Text.Regex

import BookName.Format.Util ( filterCommon, format, justAuthors, 
   titleSimple )


fmtAuthorDouble :: (MonadError String m) => Metadata -> m (String, String)
fmtAuthorDouble = format "AuthorDouble"
   ".* and .*" authorDouble
   "(.*)" titleSimple


authorDouble :: Metadata -> String
authorDouble = fmtAuthors . extractParts . head . justAuthors
   where
      extractParts (MetaCreator _ _ di) = fromJust . 
         matchRegex (mkRegex ".* ([^ ]+) and .* ([^ ]+)") $ di

      fmtAuthors (last1:last2:_) = 
         printf "%s_%s-" (filterCommon last1) (filterCommon last2)
      fmtAuthors _ = undefined
