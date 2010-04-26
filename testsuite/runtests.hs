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
   , testColon
   , testBracketTitle
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
   assertNewName "testAnonymous" bookFields expected
   where
      bookFields =
         [ "Authors: Anonymous"
         , "Title: Science Fiction Stories By Unknown Authors"
         ]
      expected =
         ( "Anonymous"
         , "Anonymous-ScienceFictionStoriesByUnknownAuthors.epub"
         )


testAuthorBasic :: Test
testAuthorBasic = TestCase $
   assertNewName "basic author" bookFields expected
   where
      bookFields =
         [ "Authors: Herman Melville"
         , "Title: Moby Dick"
         ]
      expected =
         ( "AuthorBasic"
         , "MelvilleHerman-MobyDick.epub"
         )


testAuthorDouble :: Test
testAuthorDouble = TestCase $
   assertNewName "two authors" bookFields expected
   where
      bookFields =
         [ "Authors: Kevin J. Anderson & Rebecca Moesta"
         , "Title: Rough Draft"
         ]
      expected =
         ( "AuthorDouble"
         , "Anderson_Moesta-RoughDraft.epub"
         )


testAuthorSt :: Test
testAuthorSt = TestCase $
   assertNewName "author name contains St." bookFields expected
   where
      bookFields =
         [ "Authors: Jennifer St. Clair"
         , "Title: Budget Cuts"
         ]
      expected =
         ( "AuthorSt"
         , "StClairJennifer-BudgetCuts.epub"
         )


testAuthorThird :: Test
testAuthorThird = TestCase $
   assertNewName "author name contains III" bookFields expected
   where
      bookFields =
         [ "Authors: Carlton Mellick III"
         , "Title: Sunset with a Beard"
         ]
      expected =
         ( "AuthorThird"
         , "MellickCarltonIII-SunsetWithABeard.epub"
         )


testCapsTitle :: Test
testCapsTitle = TestCase $
   assertNewName "title all caps" bookFields expected
   where
      bookFields =
         [ "Authors: Greg Bear"
         , "Title: EON"
         ]
      expected =
         ( "AuthorBasic"
         , "BearGreg-EON.epub"
         )


testColon :: Test
testColon = TestCase $
   assertNewName "colon becomes underscore" bookFields expected
   where
      bookFields =
         [ "Authors: Ed Howdershelt"
         , "Title: Book 1: 3rd World Products, Inc."
         ]
      expected =
         ( "AuthorBasic"
         , "HowdersheltEd-Book1_3rdWorldProductsInc.epub"
         )


testBracketTitle :: Test
testBracketTitle = TestCase $
   assertNewName "title with brackets" bookFields expected
   where
      bookFields =
         [ "Title: SKitty [Shipscat series #1]"
         , "Authors: Mercedes Lackey"
         ]
      expected =
         ( "AuthorBasic"
         , "LackeyMercedes-SKitty_ShipscatSeries1.epub"
         )


testMagAeon :: Test
testMagAeon = TestCase $
   assertNewName "Aeon magazine" bookFields expected
   where
      bookFields =
         [ "Authors: Aeon Authors"
         , "Title: Aeon Eight"
         ]
      expected =
         ( "MagAeon"
         , "AeonMagazine08.epub"
         )


testMagAEon :: Test
testMagAEon = TestCase $
   assertNewName "AEon magazine" bookFields expected
   where
      bookFields =
         [ "Authors: AEon Authors"
         , "Title: Aeon Thirteen"
         ]
      expected =
         ( "MagAeon"
         , "AeonMagazine13.epub"
         )


testMagApex :: Test
testMagApex = TestCase $
   assertNewName "Apex Magazine" bookFields expected
   where
      bookFields =
         [ "Authors: Apex Authors"
         , "Title: Apex Science Fiction and Horror Digest #10"
         ]
      expected =
         ( "MagNameIssue"
         , "ApexScienceFictionAndHorrorDigest10.epub"
         )


testChallengingDestinyShort :: Test
testChallengingDestinyShort = TestCase $
   assertNewName "Challenging Destiny Magazine, short" bookFields expected
   where
      bookFields =
         [ "Authors: Crystalline Sphere Authors"
         , "Title: Challenging Destiny #23"
         ]
      expected =
         ( "MagChallengingDestiny"
         , "ChallengingDestinyMagazine023.epub"
         )


testChallengingDestinyLong :: Test
testChallengingDestinyLong = TestCase $
   assertNewName "Challenging Destiny Magazine, long" bookFields expected
   where
      bookFields =
         [ "Authors: Crystalline Sphere Authors"
         , "Title: Challenging Destiny #24: August 2007"
         ]
      expected =
         ( "MagChallengingDestiny"
         , "ChallengingDestinyMagazine024.epub"
         )


testChallengingDestinyPub :: Test
testChallengingDestinyPub = TestCase $
   assertNewName "Challenging Destiny Magazine, Publishing in author" bookFields expected
   where
      bookFields =
         [ "Authors: Crystalline Sphere Publishing"
         , "Title: Challenging Destiny #18"
         ]
      expected =
         ( "MagChallengingDestiny"
         , "ChallengingDestinyMagazine018.epub"
         )


testAnalog :: Test
testAnalog = TestCase $
   assertNewName "Analog" bookFields expected
   where
      bookFields =
         [ "Authors: Dell Magazine Authors"
         , "Title: Analog SFF, July-August 2003"
         ]
      expected =
         ( "MagDell"
         , "AnalogSF2003-07_08.epub"
         )


testAsimovs :: Test
testAsimovs = TestCase $
   assertNewName "Asimovs" bookFields expected
   where
      bookFields =
         [ "Authors: Dell Magazine Authors"
         , "Title: Asimov's SF, August 2003"
         ]
      expected =
         ( "MagDell"
         , "AsimovsSF2003-08.epub"
         )


testFsfShort :: Test
testFsfShort = TestCase $
   assertNewName "FSF Magazine, short" bookFields expected
   where
      bookFields =
         [ "Authors: Spilogale Authors"
         , "Title: FSF, April 2008"
         ]
      expected =
         ( "MagFsf"
         , "FantasyScienceFiction2008-04.epub"
         )


testFsfLong :: Test
testFsfLong = TestCase $
   assertNewName "FSF Magazine, long" bookFields expected
   where
      bookFields =
         [ "Authors: Spilogale Authors"
         , "Title: FSF Magazine, April 2006"
         ]
      expected =
         ( "MagFsf"
         , "FantasyScienceFiction2006-04.epub"
         )


testMagFutureOrbits :: Test
testMagFutureOrbits = TestCase $
   assertNewName "testMagFutureOrbits" bookFields expected
   where
      bookFields =
         [ "Authors: Vander Neut Publications, L.L.C."
         , "Title: Future Orbits Issue 5, June/July 2002"
         ]
      expected =
         ( "MagFutureOrbits"
         , "FutureOrbitsMagazine05_2002-06_07.epub"
         )


testGudShort :: Test
testGudShort = TestCase $
   assertNewName "Gud Magazine, short" bookFields expected
   where
      bookFields =
         [ "Authors: GUD Magazine Authors"
         , "Title: GUD Magazine Issue 0 :: Spring 2007"
         ]
      expected =
         ( "MagGud"
         , "GUDMagazine00.epub"
         )


testGudLong :: Test
testGudLong = TestCase $
   assertNewName "Gud Magazine, long" bookFields expected
   where
      bookFields =
         [ "Authors: GUD Magazine Authors, Jeff Somers, Jeremy Shipp"
         , "Title: GUD Magazine Issue 2 :: Spring 2008"
         ]
      expected =
         ( "MagGud"
         , "GUDMagazine02.epub"
         )


testInterzoneShort :: Test
testInterzoneShort = TestCase $
   assertNewName "Interzone Magazine, short" bookFields expected
   where
      bookFields =
         [ "Authors: TTA Press Authors"
         , "Title: Interzone SFF #212"
         ]
      expected =
         ( "MagInterzone"
         , "InterzoneSFF212.epub"
         )


testInterzoneLong :: Test
testInterzoneLong = TestCase $
   assertNewName "Interzone Magazine, long" bookFields expected
   where
      bookFields =
         [ "Authors: TTA Press Authors"
         , "Title: Interzone Science Fiction and Fantasy Magazine #216"
         ]
      expected =
         ( "MagInterzone"
         , "InterzoneSFF216.epub"
         )


testNemesisShort :: Test
testNemesisShort = TestCase $
   assertNewName "Nemesis Magazine, short" bookFields expected
   where
      bookFields =
         [ "Authors: Stephen Adams"
         , "Title: Nemesis Magazine #2"
         ]
      expected =
         ( "MagNemesis"
         , "NemesisMag002.epub"
         )


testNemesisLong :: Test
testNemesisLong = TestCase $
   assertNewName "Nemesis Magazine, long" bookFields expected
   where
      bookFields =
         [ "Authors: Stephen Adams"
         , "Title: Nemesis Magazine #7: Featuring Victory Rose in Death Stalks the Ruins"
         ]
      expected =
         ( "MagNemesis"
         , "NemesisMag007.epub"
         )


testMagSomethingWicked :: Test
testMagSomethingWicked = TestCase $
   assertNewName "Something Wicked Magazine" bookFields expected
   where
      bookFields =
         [ "Authors: Something Wicked Authors"
         , "Title: Something Wicked SF and Horror Magazine #5"
         ]
      expected =
         ( "MagSomethingWicked"
         , "SomethingWickedMagazine05.epub"
         )


testSFBestOf :: Test
testSFBestOf = TestCase $
   assertNewName "Science Fiction: The Best of the Year" bookFields expected
   where
      bookFields =
         [ "Authors: Rich Horton"
         , "Title: Science Fiction: The Best of the Year, 2007 Edition"
         ]
      expected =
         ( "SFBestOf"
         , "ScienceFiction_TheBestOfTheYear2007Edition.epub"
         )


testMagBlackStatic :: Test
testMagBlackStatic = TestCase $
   assertNewName "Black Static Magazine" bookFields expected
   where
      bookFields =
         [ "Authors: TTA Press Authors"
         , "Title: Black Static Horror Magazine #5"
         ]
      expected =
         ( "MagNameIssue"
         , "BlackStaticHorrorMagazine05.epub"
         )
