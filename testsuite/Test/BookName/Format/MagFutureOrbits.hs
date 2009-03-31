-- Copyright: 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Test.BookName.Format.MagFutureOrbits
   ( testMagFutureOrbits )
   where

import Test.HUnit ( Test (..) )

import Test.BookName.Format.Util ( assertNewName )


testMagFutureOrbits :: Test
testMagFutureOrbits = TestCase $
   assertNewName "testMagFutureOrbits" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Vander Neut Publications, L.L.C."
         , "Title: Future Orbits Issue 5, June/July 2002"
         , "FreeText: 2002 by Vander Neut Publications, L.L.C."
         ]
      expected =
         ( "MagFutureOrbits"
         , "FutureOrbitsMagazine05_2002-06_07.lrf"
         )
