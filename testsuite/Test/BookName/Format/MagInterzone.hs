-- Copyright: 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Test.BookName.Format.MagInterzone
   ( testMagInterzone )
   where

import Test.HUnit ( Test (..) )

import Test.BookName.Format.Util ( assertNewName )


formatterLabel :: String
formatterLabel = "MagInterzone"


testMagInterzone :: Test
testMagInterzone = TestList
   [ testInterzoneShort
   , testInterzoneLong
   ]


testInterzoneShort :: Test
testInterzoneShort = TestCase $
   assertNewName "Interzone Magazine, short" lrfMeta expected
   where
      lrfMeta =
         [ "Author: TTA Press Authors"
         , "Title: Interzone SFF #212"
         ]
      expected =
         ( formatterLabel
         , "InterzoneSFF212.lrf"
         )


testInterzoneLong :: Test
testInterzoneLong = TestCase $
   assertNewName "Interzone Magazine, long" lrfMeta expected
   where
      lrfMeta =
         [ "Author: TTA Press Authors"
         , "Title: Interzone Science Fiction and Fantasy Magazine #216"
         ]
      expected =
         ( formatterLabel
         , "InterzoneSFF216.lrf"
         )
