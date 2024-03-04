module EpubTools.Test.EpubName.PubYear
   ( pubYearTests
   )
   where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@=?), testCase)

import Codec.Epub.Data.Metadata (DateValue (..))
import EpubTools.EpubName.Format.PubYear (extractYear)


pubYearTests :: TestTree
pubYearTests = testGroup "PubYear"
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


testExtractYear :: String -> String -> TestTree
testExtractYear dateStr expected = testCase dateStr $
   (Just expected) @=? (extractYear . DateValue $ dateStr)
