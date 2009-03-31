-- Copyright: 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Test.BookName.Format.MagSomethingWicked
   ( testMagSomethingWicked )
   where

import Test.HUnit ( Test (..) )

import Test.BookName.Format.Util ( assertNewName )


testMagSomethingWicked :: Test
testMagSomethingWicked = TestCase $
   assertNewName "testMagSomethingWicked" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Something Wicked Authors"
         , "Title: Something Wicked SF and Horror Magazine #5"
         , "FreeText: 2007 by Something Wicked and Contributing Authors"
         ]
      expected =
         ( "MagSomethingWicked"
         , "SomethingWickedMagazine05.lrf"
         )
