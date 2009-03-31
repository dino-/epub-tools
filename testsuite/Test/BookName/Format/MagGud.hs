-- Copyright: 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Test.BookName.Format.MagGud
   ( testMagGud )
   where

import Test.HUnit ( Test (..) )

import Test.BookName.Format.Util ( assertNewName )


formatterLabel :: String
formatterLabel = "MagGud"


testMagGud :: Test
testMagGud = TestList
   [ testGudShort
   , testGudLong
   ]


testGudShort :: Test
testGudShort = TestCase $
   assertNewName "Gud Magazine, short" lrfMeta expected
   where
      lrfMeta =
         [ "Author: GUD Magazine Authors"
         , "Title: GUD Magazine Issue 0 :: Spring 2007"
         , "FreeText: 2007 by GUD Magazine on behalf of contributors"
         ]
      expected =
         ( formatterLabel
         , "GUDMagazine00.lrf"
         )


testGudLong :: Test
testGudLong = TestCase $
   assertNewName "Gud Magazine, long" lrfMeta expected
   where
      lrfMeta =
         [ "Author: GUD Magazine Authors, Jeff Somers, Jeremy Shipp"
         , "Title: GUD Magazine Issue 2 :: Spring 2008"
         , "FreeText: 2008 by GUD Publishing"
         ]
      expected =
         ( formatterLabel
         , "GUDMagazine02.lrf"
         )
