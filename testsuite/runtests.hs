-- Copyright: 2007, 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Main
   where

import Control.Monad.Error
import Test.HUnit ( Counts, Test (..), assertEqual, runTestTT )
import Test.HUnit.Base ( Assertion )

import BookName.Extract
import BookName.Formatters ( tryFormatting )
import BookName.Util


assertNewName :: String
                 -> [String]
                 -> (String, String)
                 -> Assertion
assertNewName desc meta expected = do
   let fields = parseMeta ("unit test: " ++ desc) $ unlines meta
   result <- runBN $ tryFormatting fields
   let actual = either (\em -> ("NO FORMATTER", em)) id result
   assertEqual desc expected actual


main :: IO Counts
main = runTestTT tests


tests :: Test
tests = TestList
   [ testAnonymous
   , testAuthorBasic
   , testAuthorDouble
   , testAuthorSt
   , testAuthorThird
   , testCapsTitle
   , testMultPubYear
   , testColon
   , testMagAeon
   , testMagAEon
   , testMagApex
   , testChallengingDestinyShort
   , testChallengingDestinyLong
   , testChallengingDestinyPub
   , testAnalog
   , testAsimovs
   , testFsfShort
   , testFsfLong
   , testMagFutureOrbits
   , testGudShort
   , testGudLong
   , testInterzoneShort
   , testInterzoneLong
   , testNemesisShort
   , testNemesisLong
   , testMagSomethingWicked
   , testSFBestOf
   , testMagBlackStatic
   ]


testAnonymous :: Test
testAnonymous = TestCase $
   assertNewName "testAnonymous" lrfMeta expected
   where
      lrfMeta =
         [ "Authors: Anonymous"
         , "Title: Science Fiction Stories By Unknown Authors"
         , "FreeText: "
         ]
      expected =
         ( "Anonymous"
         , "Anonymous-ScienceFictionStoriesByUnknownAuthors.lrf"
         )


testAuthorBasic :: Test
testAuthorBasic = TestCase $
   assertNewName "basic author" lrfMeta expected
   where
      lrfMeta =
         [ "Authors: Herman Melville"
         , "Title: Moby Dick"
         , "FreeText: 1851 Herman Melville"
         ]
      expected =
         ( "AuthorBasic"
         , "MelvilleHerman-MobyDick_1851.lrf"
         )


testAuthorDouble :: Test
testAuthorDouble = TestCase $
   assertNewName "two authors" lrfMeta expected
   where
      lrfMeta =
         [ "Authors: Kevin J. Anderson and Rebecca Moesta"
         , "Title: Rough Draft"
         , "FreeText: 2004 Kevin J. Anderson and Rebecca Moesta"
         ]
      expected =
         ( "AuthorDouble"
         , "Anderson_Moesta-RoughDraft_2004.lrf"
         )


testAuthorSt :: Test
testAuthorSt = TestCase $
   assertNewName "author name contains St." lrfMeta expected
   where
      lrfMeta =
         [ "Authors: Jennifer St. Clair"
         , "Title: Budget Cuts"
         , "FreeText: 2004, Jennifer St. Clair"
         ]
      expected =
         ( "AuthorSt"
         , "StClairJennifer-BudgetCuts_2004.lrf"
         )


testAuthorThird :: Test
testAuthorThird = TestCase $
   assertNewName "author name contains III" lrfMeta expected
   where
      lrfMeta =
         [ "Authors: Carlton Mellick III"
         , "Title: Sunset with a Beard"
         , "FreeText: 2002 by Carlton Mellick III"
         ]
      expected =
         ( "AuthorThird"
         , "MellickCarltonIII-SunsetWithABeard_2002.lrf"
         )


testCapsTitle :: Test
testCapsTitle = TestCase $
   assertNewName "title all caps" lrfMeta expected
   where
      lrfMeta =
         [ "Authors: Greg Bear"
         , "Title: EON"
         , "FreeText: 1985 by Greg Bear"
         ]
      expected =
         ( "AuthorBasic"
         , "BearGreg-EON_1985.lrf"
         )


testMultPubYear :: Test
testMultPubYear = TestCase $
   assertNewName "more than one publication year" lrfMeta expected
   where
      lrfMeta =
         [ "Authors: Greg Bear"
         , "Title: Hegira"
         , "FreeText: 1979 by Greg Bear. Revised text copyright 1987 by Greg Bear"
         ]
      expected =
         ( "AuthorBasic"
         , "BearGreg-Hegira_1979.lrf"
         )


testColon :: Test
testColon = TestCase $
   assertNewName "colon becomes underscore" lrfMeta expected
   where
      lrfMeta =
         [ "Authors: Ed Howdershelt"
         , "Title: Book 1: 3rd World Products, Inc."
         , "FreeText: 2003 by Ed Howdershelt"
         ]
      expected =
         ( "AuthorBasic"
         , "HowdersheltEd-Book1_3rdWorldProductsInc_2003.lrf"
         )


testMagAeon :: Test
testMagAeon = TestCase $
   assertNewName "Aeon magazine" lrfMeta expected
   where
      lrfMeta =
         [ "Authors: Aeon Authors"
         , "Title: Aeon Eight"
         , "FreeText: 2006 by Scorpius Digital Publishing"
         ]
      expected =
         ( "MagAeon"
         , "AeonMagazine08.lrf"
         )


testMagAEon :: Test
testMagAEon = TestCase $
   assertNewName "AEon magazine" lrfMeta expected
   where
      lrfMeta =
         [ "Authors: AEon Authors"
         , "Title: Aeon Thirteen"
         , "FreeText: 2008 by Quintamid LLC"
         ]
      expected =
         ( "MagAeon"
         , "AeonMagazine13.lrf"
         )


testMagApex :: Test
testMagApex = TestCase $
   assertNewName "Apex Magazine" lrfMeta expected
   where
      lrfMeta =
         [ "Authors: Apex Authors"
         , "Title: Apex Science Fiction and Horror Digest #10"
         , "FreeText: 2007 by Apex Authors"
         ]
      expected =
         ( "MagNameIssue"
         , "ApexScienceFictionAndHorrorDigest10.lrf"
         )


testChallengingDestinyShort :: Test
testChallengingDestinyShort = TestCase $
   assertNewName "Challenging Destiny Magazine, short" lrfMeta expected
   where
      lrfMeta =
         [ "Authors: Crystalline Sphere Authors"
         , "Title: Challenging Destiny #23"
         , "FreeText: 2006 by Crystalline Sphere Publishing"
         ]
      expected =
         ( "MagChallengingDestiny"
         , "ChallengingDestinyMagazine023.lrf"
         )


testChallengingDestinyLong :: Test
testChallengingDestinyLong = TestCase $
   assertNewName "Challenging Destiny Magazine, long" lrfMeta expected
   where
      lrfMeta =
         [ "Authors: Crystalline Sphere Authors"
         , "Title: Challenging Destiny #24: August 2007"
         , "FreeText: 2007 by Crystalline Sphere Publishing"
         ]
      expected =
         ( "MagChallengingDestiny"
         , "ChallengingDestinyMagazine024.lrf"
         )


testChallengingDestinyPub :: Test
testChallengingDestinyPub = TestCase $
   assertNewName "Challenging Destiny Magazine, Publishing in author" lrfMeta expected
   where
      lrfMeta =
         [ "Authors: Crystalline Sphere Publishing"
         , "Title: Challenging Destiny #18"
         , "FreeText: 2004 by Crystalline Sphere Publishing"
         ]
      expected =
         ( "MagChallengingDestiny"
         , "ChallengingDestinyMagazine018.lrf"
         )


testAnalog :: Test
testAnalog = TestCase $
   assertNewName "Analog" lrfMeta expected
   where
      lrfMeta =
         [ "Authors: Dell Magazine Authors"
         , "Title: Analog SFF, July-August 2003"
         , "FreeText: 2003 by Dell Magazines"
         ]
      expected =
         ( "MagDell"
         , "AnalogSFF2003-07_08.lrf"
         )


testAsimovs :: Test
testAsimovs = TestCase $
   assertNewName "Asimovs" lrfMeta expected
   where
      lrfMeta =
         [ "Authors: Dell Magazine Authors"
         , "Title: Asimov's SF, August 2003"
         , "FreeText: 2003 by Dell Magazines"
         ]
      expected =
         ( "MagDell"
         , "AsimovsSF2003-08.lrf"
         )


testFsfShort :: Test
testFsfShort = TestCase $
   assertNewName "FSF Magazine, short" lrfMeta expected
   where
      lrfMeta =
         [ "Authors: Spilogale Authors"
         , "Title: FSF, April 2008"
         , "FreeText: 2008 by Spilogale, Inc."
         ]
      expected =
         ( "MagFsf"
         , "FantasyScienceFiction2008-04.lrf"
         )


testFsfLong :: Test
testFsfLong = TestCase $
   assertNewName "FSF Magazine, long" lrfMeta expected
   where
      lrfMeta =
         [ "Authors: Spilogale Authors"
         , "Title: FSF Magazine, April 2006"
         , "FreeText: 2006 by Spilogale, Inc."
         ]
      expected =
         ( "MagFsf"
         , "FantasyScienceFiction2006-04.lrf"
         )


testMagFutureOrbits :: Test
testMagFutureOrbits = TestCase $
   assertNewName "testMagFutureOrbits" lrfMeta expected
   where
      lrfMeta =
         [ "Authors: Vander Neut Publications, L.L.C."
         , "Title: Future Orbits Issue 5, June/July 2002"
         , "FreeText: 2002 by Vander Neut Publications, L.L.C."
         ]
      expected =
         ( "MagFutureOrbits"
         , "FutureOrbitsMagazine05_2002-06_07.lrf"
         )


testGudShort :: Test
testGudShort = TestCase $
   assertNewName "Gud Magazine, short" lrfMeta expected
   where
      lrfMeta =
         [ "Authors: GUD Magazine Authors"
         , "Title: GUD Magazine Issue 0 :: Spring 2007"
         , "FreeText: 2007 by GUD Magazine on behalf of contributors"
         ]
      expected =
         ( "MagGud"
         , "GUDMagazine00.lrf"
         )


testGudLong :: Test
testGudLong = TestCase $
   assertNewName "Gud Magazine, long" lrfMeta expected
   where
      lrfMeta =
         [ "Authors: GUD Magazine Authors, Jeff Somers, Jeremy Shipp"
         , "Title: GUD Magazine Issue 2 :: Spring 2008"
         , "FreeText: 2008 by GUD Publishing"
         ]
      expected =
         ( "MagGud"
         , "GUDMagazine02.lrf"
         )


testInterzoneShort :: Test
testInterzoneShort = TestCase $
   assertNewName "Interzone Magazine, short" lrfMeta expected
   where
      lrfMeta =
         [ "Authors: TTA Press Authors"
         , "Title: Interzone SFF #212"
         ]
      expected =
         ( "MagInterzone"
         , "InterzoneSFF212.lrf"
         )


testInterzoneLong :: Test
testInterzoneLong = TestCase $
   assertNewName "Interzone Magazine, long" lrfMeta expected
   where
      lrfMeta =
         [ "Authors: TTA Press Authors"
         , "Title: Interzone Science Fiction and Fantasy Magazine #216"
         ]
      expected =
         ( "MagInterzone"
         , "InterzoneSFF216.lrf"
         )


testNemesisShort :: Test
testNemesisShort = TestCase $
   assertNewName "Nemesis Magazine, short" lrfMeta expected
   where
      lrfMeta =
         [ "Authors: Stephen Adams"
         , "Title: Nemesis Magazine #2"
         , "FreeText: 2004 Stephen Adams"
         ]
      expected =
         ( "MagNemesis"
         , "NemesisMag002.lrf"
         )


testNemesisLong :: Test
testNemesisLong = TestCase $
   assertNewName "Nemesis Magazine, long" lrfMeta expected
   where
      lrfMeta =
         [ "Authors: Stephen Adams"
         , "Title: Nemesis Magazine #7: Featuring Victory Rose in Death Stalks the Ruins"
         , "FreeText: 2005 Stephen Adams"
         ]
      expected =
         ( "MagNemesis"
         , "NemesisMag007.lrf"
         )


testMagSomethingWicked :: Test
testMagSomethingWicked = TestCase $
   assertNewName "Something Wicked Magazine" lrfMeta expected
   where
      lrfMeta =
         [ "Authors: Something Wicked Authors"
         , "Title: Something Wicked SF and Horror Magazine #5"
         , "FreeText: 2007 by Something Wicked and Contributing Authors"
         ]
      expected =
         ( "MagSomethingWicked"
         , "SomethingWickedMagazine05.lrf"
         )


testSFBestOf :: Test
testSFBestOf = TestCase $
   assertNewName "Science Fiction: The Best of the Year" lrfMeta expected
   where
      lrfMeta =
         [ "Authors: Rich Horton"
         , "Title: Science Fiction: The Best of the Year, 2007 Edition"
         , "FreeText: 2007 by Wildside Press"
         ]
      expected =
         ( "SFBestOf"
         , "ScienceFiction_TheBestOfTheYear2007Edition.lrf"
         )


testMagBlackStatic :: Test
testMagBlackStatic = TestCase $
   assertNewName "Black Static Magazine" lrfMeta expected
   where
      lrfMeta =
         [ "Authors: TTA Press Authors"
         , "Title: Black Static Horror Magazine #5"
         ]
      expected =
         ( "MagNameIssue"
         , "BlackStaticHorrorMagazine05.lrf"
         )
