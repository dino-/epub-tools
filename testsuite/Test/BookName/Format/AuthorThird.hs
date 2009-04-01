-- Copyright: 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Test.BookName.Format.AuthorThird
   ( testAuthorThird )
   where

import Test.HUnit ( Test (..) )

import Test.BookName.Format.Util ( assertNewName )


testAuthorThird :: Test
testAuthorThird = TestCase $
   assertNewName "testAuthorThird" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Carlton Mellick III"
         , "Title: Sunset with a Beard"
         , "FreeText: 2002 by Carlton Mellick III"
         ]
      expected =
         ( "AuthorThird"
         , "MellickCarltonIII-SunsetWithABeard_2002.lrf"
         )
