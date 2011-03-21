-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.EpubName.Format.AuthorBasic
   ( fmtAuthorBasic )
   where

import Codec.Epub.Opf.Package.Metadata

import EpubTools.EpubName.Format.Util
   ( format
   , author, titleSimple
   )
import EpubTools.EpubName.Util


fmtAuthorBasic :: Metadata -> EN (String, String)
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
