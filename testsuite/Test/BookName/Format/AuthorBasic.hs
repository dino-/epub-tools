-- Copyright: 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Test.BookName.Format.AuthorBasic
   ( testAuthorBasic )
   where

import Test.HUnit ( Test (..) )

import Test.BookName.Format.Util ( assertNewName )


testAuthorBasic :: Test
testAuthorBasic = TestCase $
   assertNewName "testAuthorBasic" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Herman Melville"
         , "Title: Moby Dick"
         , "FreeText: 1851 Herman Melville"
         ]
      expected = "MelvilleHerman-MobyDick_1851.lrf"
