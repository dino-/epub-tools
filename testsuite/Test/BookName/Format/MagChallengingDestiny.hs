-- Copyright: 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Test.BookName.Format.MagChallengingDestiny
   ( testMagChallengingDestiny )
   where

import Test.HUnit ( Test (..) )

import Test.BookName.Format.Util ( assertNewName )


testMagChallengingDestiny :: Test
testMagChallengingDestiny = TestList
   [ testChallengingDestinyShort
   , testChallengingDestinyLong
   ]


testChallengingDestinyShort :: Test
testChallengingDestinyShort = TestCase $
   assertNewName "Challenging Destiny Magazine, short" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Crystalline Sphere Authors"
         , "Title: Challenging Destiny #23"
         , "FreeText: 2006 by Crystalline Sphere Publishing"
         ]
      expected = "ChallengingDestinyMagazine023.lrf"


testChallengingDestinyLong :: Test
testChallengingDestinyLong = TestCase $
   assertNewName "Challenging Destiny Magazine, long" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Crystalline Sphere Authors"
         , "Title: Challenging Destiny #24: August 2007"
         , "FreeText: 2007 by Crystalline Sphere Publishing"
         ]
      expected = "ChallengingDestinyMagazine024.lrf"
