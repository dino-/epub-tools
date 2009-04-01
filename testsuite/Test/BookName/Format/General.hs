-- Copyright: 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Test.BookName.Format.General
   ( testGeneral )
   where

import Test.HUnit ( Test (..) )

import Test.BookName.Format.Util ( assertNewName )


testGeneral :: Test
testGeneral = TestList
   [ testCapsTitle
   , testMultPubYear
   , testColon
   ]


testCapsTitle :: Test
testCapsTitle = TestCase $
   assertNewName "All caps title" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Greg Bear"
         , "Title: EON"
         , "FreeText: 1985 by Greg Bear"
         ]
      expected =
         ( "AuthorBasic"
         , "BearGreg-EON_1985.lrf"
         )


testMultPubYear :: Test
testMultPubYear = TestCase $
   assertNewName "More than one publication year" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Greg Bear"
         , "Title: Hegira"
         , "FreeText: 1979 by Greg Bear. Revised text copyright 1987 by Greg Bear"
         ]
      expected =
         ( "AuthorBasic"
         , "BearGreg-Hegira_1979.lrf"
         )


testColon :: Test
testColon = TestCase $
   assertNewName "colon becomes underscore" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Ed Howdershelt"
         , "Title: Book 1: 3rd World Products, Inc."
         , "FreeText: 2003 by Ed Howdershelt"
         ]
      expected =
         ( "AuthorBasic"
         , "HowdersheltEd-Book1_3rdWorldProductsInc_2003.lrf"
         )
