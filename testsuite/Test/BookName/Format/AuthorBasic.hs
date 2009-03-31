-- Copyright: 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Test.BookName.Format.AuthorBasic
   ( testAuthorBasic )
   where

import Test.HUnit ( Test (..) )

import Test.BookName.Format.Util ( assertNewName )


formatterLabel :: String
formatterLabel = "AuthorBasic"


testAuthorBasic :: Test
testAuthorBasic = TestList
   [ testBasic
   , testCapsTitle
   ]


testBasic :: Test
testBasic = TestCase $
   assertNewName "Basic single author" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Herman Melville"
         , "Title: Moby Dick"
         , "FreeText: 1851 Herman Melville"
         ]
      expected =
         ( formatterLabel
         , "MelvilleHerman-MobyDick_1851.lrf"
         )


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
         ( formatterLabel
         , "BearGreg-EON_1985.lrf"
         )
