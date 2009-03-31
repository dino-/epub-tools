-- Copyright: 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Test.BookName.Format.Anonymous
   ( testAnonymous )
   where

import Test.HUnit ( Test (..) )

import Test.BookName.Format.Util ( assertNewName )


testAnonymous :: Test
testAnonymous = TestCase $
   assertNewName "testAnonymous" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Anonymous"
         , "Title: Science Fiction Stories By Unknown Authors"
         , "FreeText: "
         ]
      expected = "Anonymous-ScienceFictionStoriesByUnknownAuthors.lrf"
