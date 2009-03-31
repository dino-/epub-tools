{-# LANGUAGE FlexibleContexts #-}

module BookName.Format.AuthorBasic
   ( fmtAuthorBasic )
   where

import Control.Monad.Error

import BookName.Format.Util
   ( format
   , authorSingle, titleSimple
   )
import BookName.Util ( Fields )


fmtAuthorBasic :: (MonadError String m) => Fields -> m (String, String)
fmtAuthorBasic = format "AuthorBasic"
   "(.*) ([^ ]+)$" authorSingle
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
