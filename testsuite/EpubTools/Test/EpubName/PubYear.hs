-- Copyright: 2012 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.Test.EpubName.PubYear
   ( pubYearTests
   )
   where

import Test.HUnit ( Test (..), assertEqual )

import EpubTools.EpubName.Format.PubYear


pubYearTests :: Test
pubYearTests = TestLabel "PubYear" $ TestList
   [ testExtractYear "2001-03-25T00:00:00" "2001"
   , testExtractYear "2007" "2007"
   , testExtractYear "Sat Nov 03 18:49:50 +0100 2007" "2007"
   , testExtractYear "2001-03-25 05:00:00+00:00" "2001"
   , testExtractYear "2009-11-19T07:00:00+00:00" "2009"
   , testExtractYear "2010-09-21" "2010"
   , testExtractYear "2010-02-14T21:47:47.682882+00:00" "2010"
   , testExtractYear "2011-11-16T09:19:50-0500" "2011"
   , testExtractYear "2011-11-16T14:19:52Z" "2011"
   , testExtractYear "Tue, 29 Nov 2011 06:21:19 +0000" "2011"
   ]


testExtractYear :: String -> String -> Test
testExtractYear src expected = TestCase $
   assertEqual src (extractYear src) (Just expected)
