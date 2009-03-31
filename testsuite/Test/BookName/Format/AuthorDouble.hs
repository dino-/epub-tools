-- Copyright: 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Test.BookName.Format.AuthorDouble
   ( testAuthorDouble )
   where

import Test.HUnit ( Test (..) )

import Test.BookName.Format.Util ( assertNewName )


testAuthorDouble :: Test
testAuthorDouble = TestCase $
   assertNewName "Two authors" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Kevin J. Anderson and Rebecca Moesta"
         , "Title: Rough Draft"
         , "FreeText: 2004 Kevin J. Anderson and Rebecca Moesta"
         ]
      expected =
         ( "AuthorDouble"
         , "Anderson_Moesta-RoughDraft_2004.lrf"
         )
