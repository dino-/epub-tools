-- Copyright: 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Test.BookName.Format.MagInterzone
   ( testMagInterzone )
   where

import Test.HUnit ( Test (..) )

import Test.BookName.Format.Util ( assertNewName )


testMagInterzone :: Test
testMagInterzone = TestCase $
   assertNewName "testMagInterzone" lrfMeta expected
   where
      lrfMeta =
         [ "Author: TTA Press Authors"
         , "Title: Interzone SFF #212"
         ]
      expected = "InterzoneSFF212.lrf"
