-- Copyright: 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Test.BookName.Format.SFBestOf
   ( testSFBestOf )
   where

import Test.HUnit ( Test (..) )

import Test.BookName.Format.Util ( assertNewName )


testSFBestOf :: Test
testSFBestOf = TestCase $
   assertNewName "testSFBestOf" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Rich Horton"
         , "Title: Science Fiction: The Best of the Year, 2007 Edition"
         , "FreeText: 2007 by Wildside Press"
         ]
      expected =
         ( "SFBestOf"
         , "ScienceFiction_TheBestOfTheYear2007Edition.lrf"
         )
