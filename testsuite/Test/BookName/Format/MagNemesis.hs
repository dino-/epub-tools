-- Copyright: 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Test.BookName.Format.MagNemesis
   ( testMagNemesis )
   where

import Test.HUnit ( Test (..) )

import Test.BookName.Format.Util ( assertNewName )


formatterLabel :: String
formatterLabel = "MagNemesis"


testMagNemesis :: Test
testMagNemesis = TestList
   [ testNemesisShort
   , testNemesisLong
   ]


testNemesisShort :: Test
testNemesisShort = TestCase $
   assertNewName "Nemesis Magazine, short" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Stephen Adams"
         , "Title: Nemesis Magazine #2"
         , "FreeText: 2004 Stephen Adams"
         ]
      expected =
         ( formatterLabel
         , "NemesisMag002.lrf"
         )


testNemesisLong :: Test
testNemesisLong = TestCase $
   assertNewName "Nemesis Magazine, long" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Stephen Adams"
         , "Title: Nemesis Magazine #7: Featuring Victory Rose in Death Stalks the Ruins"
         , "FreeText: 2005 Stephen Adams"
         ]
      expected =
         ( formatterLabel
         , "NemesisMag007.lrf"
         )
