-- Copyright: 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Test.BookName.Format.MagAeon
   ( testMagAeon )
   where

import Test.HUnit ( Test (..) )

import Test.BookName.Format.Util ( assertNewName )


testMagAeon :: Test
testMagAeon = TestList $
   [ testAeon
   , testAEon
   ]


testAeon :: Test
testAeon = TestCase $
   assertNewName "testAeon" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Aeon Authors"
         , "Title: Aeon Eight"
         , "FreeText: 2006 by Scorpius Digital Publishing"
         ]
      expected = "AeonMagazine08.lrf"


testAEon :: Test
testAEon = TestCase $
   assertNewName "testAEon" lrfMeta expected
   where
      lrfMeta =
         [ "Author: AEon Authors"
         , "Title: Aeon Thirteen"
         , "FreeText: 2008 by Quintamid LLC"
         ]
      expected = "AeonMagazine13.lrf"
