-- Copyright: 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Test.BookName.Format.MagDell
   ( testMagDell )
   where

import Test.HUnit ( Test (..) )

import Test.BookName.Format.Util ( assertNewName )


testMagDell :: Test
testMagDell = TestList
   [ testAnalog
   , testAsimovs
   ]


testAnalog :: Test
testAnalog = TestCase $
   assertNewName "Analog" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Dell Magazine Authors"
         , "Title: Analog SFF, July-August 2003"
         , "FreeText: 2003 by Dell Magazines"
         ]
      expected = "AnalogSFF2003-07_08.lrf"


testAsimovs :: Test
testAsimovs = TestCase $
   assertNewName "Asimovs" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Dell Magazine Authors"
         , "Title: Asimov's SF, August 2003"
         , "FreeText: 2003 by Dell Magazines"
         ]
      expected = "AsimovsSF2003-08.lrf"
