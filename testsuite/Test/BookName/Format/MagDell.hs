-- Copyright: 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Test.BookName.Format.MagDell
   ( testMagDell )
   where

import Test.HUnit ( Test (..) )

import BookName.Format.MagDell
import Test.BookName.Format.Util ( assertNewName )


testMagDell :: Test
testMagDell = TestCase $
   assertNewName "testMagDell" fmtMagDell lrfMeta expected
   where
      lrfMeta =
         [ "Author: Dell Magazine Authors"
         , "Title: Analog SFF, July-August 2003"
         , "FreeText: 2003 by Dell Magazines"
         ]
      expected = "AnalogSFF2003-07_08.lrf"
