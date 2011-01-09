-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.AuthorBasic
   ( fmtAuthorBasic )
   where

import Codec.Epub.Opf.Package.Metadata
import Control.Monad.Error

import BookName.Format.Util
   ( format
   , author, titleSimple
   )


fmtAuthorBasic :: (MonadError String m) => Metadata -> m (String, String)
fmtAuthorBasic = format "AuthorBasic"
   ".*" author
   "(.*)" titleSimple


{-
authorPatterns :: [(String, [String] -> String)]
authorPatterns =
   [ ( ".* ([^ ]+) and .* ([^ ]+)", authorDouble )
   , ( "(.*)(Anonymous)", authorSingle )
   , ( "(.*) ([^ ]+ III)$", authorSingle )
   , ( "(.*) ([^ ]+ Jr\\.)$", authorSingle )
   , ( "(.*) (St\\. [^ ]+)$", authorSingle )
   , ( "(.*) ([^ ]+)$", authorSingle )
   ]
-}
