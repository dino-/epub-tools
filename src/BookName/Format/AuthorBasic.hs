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
