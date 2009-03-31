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