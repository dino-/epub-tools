-- Copyright: 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Test.BookName.Format.MagAnalog
   ( testMagAnalog )
   where

import Test.HUnit ( Test (..) )

import BookName.Format.MagAnalog
import Test.BookName.Format.Util ( assertNewName )


testMagAnalog :: Test
testMagAnalog = TestCase $
   assertNewName "testMagAnalog" fmtMagAnalog lrfMeta expected
   where
      lrfMeta =
         [ "Author: Dell Magazine Authors"
         , "Title: Analog SFF, July-August 2003"
         , "FreeText: 2003 by Dell Magazines"
         ]
      expected = "AnalogSFF2003-07_08.lrf"
