-- Copyright: 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Test.BookName.Format.MagApex
   ( testMagApex )
   where

import Test.HUnit ( Test (..) )

import BookName.Format.MagApex
import Test.BookName.Format.Util ( assertNewName )


testMagApex :: Test
testMagApex = TestCase $
   assertNewName "testMagApex" fmtMagApex lrfMeta expected
   where
      lrfMeta =
         [ "Author: Apex Authors"
         , "Title: Apex Science Fiction and Horror Digest #10"
         , "FreeText: 2007 by Apex Authors"
         ]
      expected = "ApexScienceFictionAndHorrorDigest10.lrf"