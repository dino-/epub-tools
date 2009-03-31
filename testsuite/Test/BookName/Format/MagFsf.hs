-- Copyright: 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Test.BookName.Format.MagFsf
   ( testMagFsf )
   where

import Test.HUnit ( Test (..) )

import Test.BookName.Format.Util ( assertNewName )


formatterLabel :: String
formatterLabel = "MagFsf"


testMagFsf :: Test
testMagFsf = TestList
   [ testFsfShort
   , testFsfLong
   ]


testFsfShort :: Test
testFsfShort = TestCase $
   assertNewName "FSF Magazine, short" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Spilogale Authors"
         , "Title: FSF, April 2008"
         , "FreeText: 2008 by Spilogale, Inc."
         ]
      expected =
         ( formatterLabel
         , "FantasyScienceFiction2008-04.lrf"
         )


testFsfLong :: Test
testFsfLong = TestCase $
   assertNewName "FSF Magazine, long" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Spilogale Authors"
         , "Title: FSF Magazine, April 2006"
         , "FreeText: 2006 by Spilogale, Inc."
         ]
      expected =
         ( formatterLabel
         , "FantasyScienceFiction2006-04.lrf"
         )
