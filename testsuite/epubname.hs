-- Copyright: 2008-2011 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Main
   where

import Codec.Epub.Opf.Package.Metadata
import System.Exit
import Test.HUnit ( Counts (..), Test (..), assertEqual, runTestTT )
import Test.HUnit.Base ( Assertion )

import EpubTools.EpubName.Format ( tryFormatting )
import EpubTools.EpubName.Opts
import EpubTools.EpubName.Util


main :: IO ()
main = do
   counts <- runTestTT tests
   exit $ testsPassed counts


exit :: Bool -> IO ()
exit True  = exitWith ExitSuccess
exit False = exitWith $ ExitFailure 1


testsPassed :: Counts -> Bool
testsPassed (Counts _ _ e f) = (e == 0) && (f == 0)


tests :: Test
tests = TestList
   [ testAuthorMinimal
   , testAuthorOneName
   , testAuthorRole
   , testAuthorFileas
   , testAuthorFull
   , testAuthorSeveral
   , testSeveralAuthors
   , testNoAuthor
   , testCreatorsNoAuthor
   , testCreatorsNoAuthorPubDate
   , testCapsTitle
   , testColon
   , testBracketTitle
   , testNoTitle
   , testAllPunctuation
   , testPubYear
   , testPubYearUnwanted
   , testMagAeon
   , testMagAEon
   , testMagApexLong
   , testMagApexShort
   , testChallengingDestinyShort
   , testChallengingDestinyLong
   , testAnalog
   , testAsimovs
   , testFantasyMagazine
   , testFsfShort
   , testFsfLong
   , testFsfAmpersand
   , testMagFutureOrbits
   , testGudShort
   , testGudLong
   , testGudVeryLong
   , testInterzoneShort
   , testInterzoneLong
   , testNemesisShort
   , testNemesisLong
   , testMagSomethingWicked
   , testSFBestOf
   , testMagBlackStatic
   , testRageMachineMag
   , testEclipseMag
   , testBcs
   , testBkpFileAs
   , testBkpText
   , testBkpMissing
   , testMagUniverse
   , testMagClarkesworld
   , testLightspeedDate
   , testLightspeedIssue
   , testMagWeirdTales
   ]


assertNewNameOpts :: Options -> String -> Metadata 
   -> (String, String) -> Assertion
assertNewNameOpts opts desc meta expected = do
   result <- runEN opts $ tryFormatting "" meta
   let actual = either (\em -> ("NO FORMATTER", em)) id result
   assertEqual desc expected actual


assertNewName :: String -> Metadata -> (String, String) -> Assertion
assertNewName = assertNewNameOpts defaultOptions


testAuthorMinimal :: Test
testAuthorMinimal = TestCase $
   assertNewName "minimal author" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing "Herman Melville"]
         , metaTitles = [MetaTitle Nothing "Moby Dick"]
         }
      expected =
         ( "book"
         , "MelvilleHerman-MobyDick.epub"
         )


testAuthorOneName :: Test
testAuthorOneName = TestCase $
   assertNewName "author is a single word" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing "Melville"]
         , metaTitles = [MetaTitle Nothing "Moby Dick"]
         }
      expected =
         ( "book"
         , "Melville-MobyDick.epub"
         )


testAuthorRole :: Test
testAuthorRole = TestCase $
   assertNewName "author with role" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator (Just "aut") Nothing
            "Herman Melville"]
         , metaTitles = [MetaTitle Nothing "Moby Dick"]
         }
      expected =
         ( "book"
         , "MelvilleHerman-MobyDick.epub"
         )


testAuthorFileas :: Test
testAuthorFileas = TestCase $
   assertNewName "author with file-as" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing
            (Just "Melville, Herman")
            "Herman Melville"]
         , metaTitles = [MetaTitle Nothing "Moby Dick"]
         }
      expected =
         ( "book"
         , "MelvilleHerman-MobyDick.epub"
         )


testAuthorFull :: Test
testAuthorFull = TestCase $
   assertNewName "author fully filled out" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator (Just "aut")
            (Just "Melville, Herman")
            "Herman Melville"]
         , metaTitles = [MetaTitle Nothing "Moby Dick"]
         }
      expected =
         ( "book"
         , "MelvilleHerman-MobyDick.epub"
         )


testAuthorSeveral :: Test
testAuthorSeveral = TestCase $
   assertNewName "several authors" meta expected
   where
      meta = emptyMetadata
         { metaCreators =
            [ MetaCreator (Just "aut") Nothing
               "James Patrick Kelly"
            , MetaCreator (Just "aut") (Just "Kessel, John")
               "John Kessel"
            , MetaCreator (Just "aut") Nothing
               "Jonathan Lethem"
            ]
         , metaTitles = [MetaTitle Nothing
            "Ninety Percent of Everything"]
         }
      expected =
         ( "book"
         , "Kelly_Kessel_Lethem-NinetyPercentOfEverything.epub"
         )


testNoAuthor :: Test
testNoAuthor = TestCase $
   assertNewName "no creator(s) at all" meta expected
   where
      meta = emptyMetadata
         { metaCreators = []
         , metaTitles = [MetaTitle Nothing
            "Some Collection of Fine Stories, Volume 1"]
         }
      expected =
         ( "book"
         , "SomeCollectionOfFineStoriesVolume1.epub"
         )


testCreatorsNoAuthor :: Test
testCreatorsNoAuthor = TestCase $
   assertNewName "creators, but no author(s) at all" meta expected
   where
      meta = emptyMetadata
         { metaCreators =
            [ MetaCreator (Just "edt") Nothing
               "Graham Spindlewest"
            , MetaCreator (Just "ill") Nothing
               "Eva Tunglewacker"
            ]
         , metaTitles = [MetaTitle Nothing
            "Some Collection of Fine Stories, Volume 1"]
         }
      expected =
         ( "book"
         , "SomeCollectionOfFineStoriesVolume1.epub"
         )


testCreatorsNoAuthorPubDate :: Test
testCreatorsNoAuthorPubDate = TestCase $
   assertNewName "creators, but no author(s) at all, with pub date" 
      meta expected
   where
      meta = emptyMetadata
         { metaCreators =
            [ MetaCreator (Just "edt") Nothing
               "Graham Spindlewest"
            , MetaCreator (Just "ill") Nothing
               "Eva Tunglewacker"
            ]
         , metaTitles = [MetaTitle Nothing
            "Some Collection of Fine Stories, Volume 1"]
         , metaDates = [MetaDate (Just "original-publication")
            "2008"]
         }
      expected =
         ( "book"
         , "SomeCollectionOfFineStoriesVolume1_2008.epub"
         )


testSeveralAuthors :: Test
testSeveralAuthors = TestCase $
   assertNewName "more than one author separated by & and/or and" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing
            "Yvonne Q. Anderson & Eva Tunglewacker and Jefferson Milner"]
         , metaTitles = [MetaTitle Nothing "Big Trouble"]
         }
      expected =
         ( "book"
         , "Anderson_Tunglewacker_Milner-BigTrouble.epub"
         )


testCapsTitle :: Test
testCapsTitle = TestCase $
   assertNewName "title all caps" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing "Greg Bear"]
         , metaTitles = [MetaTitle Nothing "EON"]
         }
      expected =
         ( "book"
         , "BearGreg-Eon.epub"
         )


testColon :: Test
testColon = TestCase $
   assertNewName "colon becomes underscore" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing "Ed Howdershelt"]
         , metaTitles = [MetaTitle Nothing 
            "Book 1: 3rd World Products, Inc."]
         }
      expected =
         ( "book"
         , "HowdersheltEd-Book1_3rdWorldProductsInc.epub"
         )


testBracketTitle :: Test
testBracketTitle = TestCase $
   assertNewName "title with brackets" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing "Mercedes Lackey"]
         , metaTitles = [MetaTitle Nothing "SKitty [Shipscat series #1]"]
         }
      expected =
         ( "book"
         , "LackeyMercedes-Skitty_ShipscatSeries1.epub"
         )


testNoTitle :: Test
testNoTitle = TestCase $
   assertNewName "missing title" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing
            "Nobody McCrankypants"]
         }
      expected =
         ( "NO FORMATTER"
         , " [ERROR No formatter found]"
         )


testAllPunctuation :: Test
testAllPunctuation = TestCase $
   assertNewName "big test of all punctuation" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing
            "Dino Morelli"]
         , metaTitles = [MetaTitle Nothing
            "The *crazy*: Sand-box. Of Smedley's discontent, fear & Malnourishment? (Maybe not!); [Part #2]"]
         }
      expected =
         ( "book"
         , "MorelliDino-TheCrazy_SandBoxOfSmedleysDiscontentFearAndMalnourishmentMaybeNot_Part2.epub"
         )


testPubYear :: Test
testPubYear = TestCase $
   assertNewName "book with a publication year" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing "Jim Jones"]
         , metaTitles = [MetaTitle Nothing "A Timeless Story"]
         , metaDates = [MetaDate (Just "original-publication") "2003"]
         }
      expected =
         ( "book"
         , "JonesJim-ATimelessStory_2003.epub"
         )


testPubYearUnwanted :: Test
testPubYearUnwanted = TestCase $
   assertNewNameOpts opts
      "book with a publication year but we don't want"
      meta expected
   where
      opts = defaultOptions { optYear = False }
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing "Jim Jones"]
         , metaTitles = [MetaTitle Nothing "A Timeless Story"]
         , metaDates = [MetaDate (Just "original-publication") "2003"]
         }
      expected =
         ( "book"
         , "JonesJim-ATimelessStory.epub"
         )


testMagAeon :: Test
testMagAeon = TestCase $
   assertNewName "Aeon magazine" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing "Aeon Authors"]
         , metaTitles = [MetaTitle Nothing "Aeon Eight"]
         }
      expected =
         ( "magAeon"
         , "AeonMagazine08.epub"
         )


testMagAEon :: Test
testMagAEon = TestCase $
   assertNewName "AEon magazine" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing "AEon Authors"]
         , metaTitles = [MetaTitle Nothing "Aeon Thirteen"]
         }
      expected =
         ( "magAeon"
         , "AeonMagazine13.epub"
         )


testMagApexLong :: Test
testMagApexLong = TestCase $
   assertNewName "Apex Magazine, older, long title" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator (Just "aut") Nothing
            "Apex Authors"]
         , metaTitles = [MetaTitle Nothing
            "Apex Science Fiction and Horror Digest #9"]
         }
      expected =
         ( "magApex"
         , "ApexMagazine009.epub"
         )


testMagApexShort :: Test
testMagApexShort = TestCase $
   assertNewName "Apex Magazine, newer, short title" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator (Just "aut") Nothing
            "Apex Authors"]
         , metaTitles = [MetaTitle Nothing
            "Apex Magazine Issue 17"]
         }
      expected =
         ( "magApex"
         , "ApexMagazine017.epub"
         )


testChallengingDestinyShort :: Test
testChallengingDestinyShort = TestCase $
   assertNewName "Challenging Destiny Magazine, short title" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing
            "Crystalline Sphere Authors"]
         , metaTitles = [MetaTitle Nothing
            "Challenging Destiny #23"]
         }
      expected =
         ( "magChallengingDestiny"
         , "ChallengingDestinyMagazine023.epub"
         )


testChallengingDestinyLong :: Test
testChallengingDestinyLong = TestCase $
   assertNewName "Challenging Destiny Magazine, long title" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing 
            "Crystalline Sphere Authors"]
         , metaTitles = [MetaTitle Nothing 
            "Challenging Destiny #24: August 2007"]
         }
      expected =
         ( "magChallengingDestiny"
         , "ChallengingDestinyMagazine024.epub"
         )


testAnalog :: Test
testAnalog = TestCase $
   assertNewName "Analog" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing
            "Dell Magazine Authors"]
         , metaTitles = [MetaTitle Nothing
            "Analog SFF, July-August 2003"]
         }
      expected =
         ( "magAnalog"
         , "AnalogSF2003-07_08.epub"
         )


testAsimovs :: Test
testAsimovs = TestCase $
   assertNewName "Asimovs" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing
            "Dell Magazine Authors"]
         , metaTitles = [MetaTitle Nothing
            "Asimov's SF, August 2003"]
         }
      expected =
         ( "magAnalog"
         , "AsimovsSF2003-08.epub"
         )

testFantasyMagazine :: Test
testFantasyMagazine = TestCase $
   assertNewName "Fantasy Magazine" meta expected
   where
      meta = emptyMetadata
         { metaCreators =
            [ MetaCreator (Just "aut") (Just "Howard, Kat & Tanzer, Molly & Beagle, Peter S. & Howard, Jonathan L. & Valentine, Genevieve & Vaughn, Carrie & Pilinovsky, Helen") "Kat Howard"
            , MetaCreator (Just "aut") Nothing "Molly Tanzer"
            , MetaCreator (Just "aut") Nothing "Peter S. Beagle"
            , MetaCreator (Just "aut") Nothing "Jonathan L. Howard"
            , MetaCreator (Just "aut") Nothing "Genevieve Valentine"
            , MetaCreator (Just "aut") Nothing "Carrie Vaughn"
            , MetaCreator (Just "aut") Nothing "Helen Pilinovsky"
            ]
         , metaTitles = [MetaTitle Nothing
            "Fantasy Magazine Issue 49"]
         }
      expected =
         ( "magFantasyMag"
         , "FantasyMagazine049.epub"
         )


testFsfShort :: Test
testFsfShort = TestCase $
   assertNewName "FSF Magazine, short" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing
            "Spilogale Authors"]
         , metaTitles = [MetaTitle Nothing "FSF, April 2008"]
         }
      expected =
         ( "magFsf"
         , "FantasyScienceFiction2008-04.epub"
         )


testFsfLong :: Test
testFsfLong = TestCase $
   assertNewName "FSF Magazine, long" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing 
            "Spilogale Authors"]
         , metaTitles = [MetaTitle Nothing 
            "FSF Magazine, April 2006"]
         }
      expected =
         ( "magFsf"
         , "FantasyScienceFiction2006-04.epub"
         )


testFsfAmpersand :: Test
testFsfAmpersand = TestCase $
   assertNewName "FSF Magazine, ampersand" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing
            "Spilogale"]
         , metaTitles = [MetaTitle Nothing "F & SF Dec 2003"]
         }
      expected =
         ( "magFsf"
         , "FantasyScienceFiction2003-12.epub"
         )


testMagFutureOrbits :: Test
testMagFutureOrbits = TestCase $
   assertNewName "testMagFutureOrbits" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing 
            "Vander Neut Publications, L.L.C."]
         , metaTitles = [MetaTitle Nothing 
            "Future Orbits Issue 5, June/July 2002"]
         }
      expected =
         ( "magFutureOrbits"
         , "FutureOrbitsMagazine05_2002-06_07.epub"
         )


testGudShort :: Test
testGudShort = TestCase $
   assertNewName "Gud Magazine, short" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing 
            "GUD Magazine Authors"]
         , metaTitles = [MetaTitle Nothing 
            "GUD Magazine Issue 0 :: Spring 2007"]
         }
      expected =
         ( "magGud"
         , "GUDMagazine00.epub"
         )


testGudLong :: Test
testGudLong = TestCase $
   assertNewName "Gud Magazine, long" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing 
            "GUD Magazine Authors, Jeff Somers, Jeremy Shipp"]
         , metaTitles = [MetaTitle Nothing 
            "GUD Magazine Issue 2 :: Spring 2008"]
         }
      expected =
         ( "magGud"
         , "GUDMagazine02.epub"
         )


testGudVeryLong :: Test
testGudVeryLong = TestCase $
   assertNewName "Gud Magazine, very long" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing 
            "GUD Magazine Authors"]
         , metaTitles = [MetaTitle Nothing 
            "Greatest Uncommon Denominator Magazine Issue 4 :: Spring 2009"]
         }
      expected =
         ( "magGud"
         , "GUDMagazine04.epub"
         )


testInterzoneShort :: Test
testInterzoneShort = TestCase $
   assertNewName "Interzone Magazine, short" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing 
            "TTA Press Authors"]
         , metaTitles = [MetaTitle Nothing 
            "Interzone SFF #212"]
         }
      expected =
         ( "magInterzone"
         , "InterzoneSFF212.epub"
         )


testInterzoneLong :: Test
testInterzoneLong = TestCase $
   assertNewName "Interzone Magazine, long" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing 
            "TTA Press Authors"]
         , metaTitles = [MetaTitle Nothing 
            "Interzone Science Fiction and Fantasy Magazine #216"]
         }
      expected =
         ( "magInterzone"
         , "InterzoneSFF216.epub"
         )


testNemesisShort :: Test
testNemesisShort = TestCase $
   assertNewName "Nemesis Magazine, short" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing 
            "Stephen Adams"]
         , metaTitles = [MetaTitle Nothing 
            "Nemesis Magazine #2"]
         }
      expected =
         ( "magNemesis"
         , "NemesisMag002.epub"
         )


testNemesisLong :: Test
testNemesisLong = TestCase $
   assertNewName "Nemesis Magazine, long" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing 
            "Stephen Adams"]
         , metaTitles = [MetaTitle Nothing 
            "Nemesis Magazine #7: Featuring Victory Rose in Death Stalks the Ruins"]
         }
      expected =
         ( "magNemesis"
         , "NemesisMag007.epub"
         )


testMagSomethingWicked :: Test
testMagSomethingWicked = TestCase $
   assertNewName "Something Wicked Magazine" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing 
            "Something Wicked Authors"]
         , metaTitles = [MetaTitle Nothing 
            "Something Wicked SF and Horror Magazine #5"]
         }
      expected =
         ( "magSomethingWicked"
         , "SomethingWickedMagazine05.epub"
         )


testSFBestOf :: Test
testSFBestOf = TestCase $
   assertNewName "Science Fiction: The Best of the Year" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing 
            "Rich Horton"]
         , metaTitles = [MetaTitle Nothing 
            "Science Fiction: The Best of the Year, 2007 Edition"]
         }
      expected =
         ( "sfBestOfYear"
         , "ScienceFiction_TheBestOfTheYear2007Edition.epub"
         )


testMagBlackStatic :: Test
testMagBlackStatic = TestCase $
   assertNewName "Black Static Magazine" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator Nothing Nothing 
            "TTA Press Authors"]
         , metaTitles = [MetaTitle Nothing 
            "Black Static Horror Magazine #5"]
         }
      expected =
         ( "magBlackStatic"
         , "BlackStaticHorrorMagazine05.epub"
         )


testRageMachineMag :: Test
testRageMachineMag = TestCase $
   assertNewName "Rage Machine Magazine" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator (Just "edt") Nothing 
            "G. W. Thomas"]
         , metaTitles = [MetaTitle Nothing 
            "Rage Machine Magazine #1--December 2005"]
         }
      expected =
         ( "magRageMachine"
         , "RageMachineMagazine1_2005-12.epub"
         )


testEclipseMag :: Test
testEclipseMag = TestCase $
   assertNewName "Eclipse Magazine" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator (Just "aut")
            (Just "Strahan, Jonathan")
            "Jonathan Strahan"]
         , metaTitles = [MetaTitle Nothing 
            "Eclipse One"]
         }
      expected =
         ( "magEclipse"
         , "Eclipse01.epub"
         )


testBcs :: Test
testBcs = TestCase $
   assertNewName "Beneath Ceaseless Skies Magazine" meta expected
   where
      meta = emptyMetadata
         { metaCreators = 
            [ MetaCreator (Just "aut")
               (Just "Tidwell, Erin A.")
               "Hoover, Kenneth Mark"
            , MetaCreator (Just "aut")
               (Just "Tidwell, Erin A.")
               "Tidwell, Erin A."
            ]
         , metaTitles = [MetaTitle Nothing 
            "Beneath Ceaseless Skies #32"]
         }
      expected =
         ( "magBcs"
         , "BeneathCeaselessSkies_Issue032.epub"
         )


testBkpFileAs :: Test
testBkpFileAs = TestCase $
   assertNewNameOpts opts 
      "book publisher suffix requested and present in file-as"
      meta expected
   where
      opts = defaultOptions { optPublisher = True }
      meta = emptyMetadata
         { metaCreators = [MetaCreator (Just "aut") Nothing
            "Herman Melville"]
         , metaContributors = [MetaCreator (Just "bkp") (Just "acme") 
            "Acme Publishing, Inc."]
         , metaTitles = [MetaTitle Nothing "Moby Dick"]
         }
      expected =
         ( "book"
         , "MelvilleHerman-MobyDick_acme.epub"
         )


testBkpText :: Test
testBkpText = TestCase $
   assertNewNameOpts opts 
      "book publisher suffix requested and present in text"
      meta expected
   where
      opts = defaultOptions { optPublisher = True }
      meta = emptyMetadata
         { metaCreators = [MetaCreator (Just "aut") Nothing
            "Herman Melville"]
         , metaContributors = [MetaCreator (Just "bkp") Nothing
            "Acme Publishing, Inc."]
         , metaTitles = [MetaTitle Nothing "Moby Dick"]
         }
      expected =
         ( "book"
         , "MelvilleHerman-MobyDick.epub"
         )


testBkpMissing :: Test
testBkpMissing = TestCase $
   assertNewNameOpts opts 
      "book publisher suffix requested and not present"
      meta expected
   where
      opts = defaultOptions { optPublisher = True }
      meta = emptyMetadata
         { metaCreators = [MetaCreator (Just "aut") Nothing
            "Herman Melville"]
         , metaTitles = [MetaTitle Nothing "Moby Dick"]
         }
      expected =
         ( "book"
         , "MelvilleHerman-MobyDick.epub"
         )


testMagUniverse :: Test
testMagUniverse = TestCase $
   assertNewNameOpts opts 
      "Jim Baen's Universe Magazine"
      meta expected
   where
      opts = defaultOptions { optPublisher = True }
      meta = emptyMetadata
         { metaCreators = [MetaCreator (Just "aut") Nothing
            "Jim Baen's Universe"]
         , metaTitles = [MetaTitle Nothing "Jim Baen's Universe-Vol 4 Num 6"]
         }
      expected =
         ( "magUniverse"
         , "JimBaensUniverseVol04Num06.epub"
         )


testMagClarkesworld :: Test
testMagClarkesworld = TestCase $
   assertNewName "Clarkesworld Magazine" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator (Just "aut")
            (Just "Kowal, Mary Robinette")
            "Mary Robinette Kowal"]
         , metaTitles = [MetaTitle Nothing
            "Clarkesworld Magazine - Issue 21"]
         }
      expected =
         ( "magClarkesworld"
         , "Clarkesworld021.epub"
         )


testLightspeedDate :: Test
testLightspeedDate = TestCase $
   assertNewName "Lightspeed Magazine, date in title" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator (Just "aut") Nothing
            "Edited by John Joseph Adams"]
         , metaTitles = [MetaTitle Nothing 
            "Lightspeed Magazine, June 2010"]
         }
      expected =
         ( "magLightspeedDate"
         , "Lightspeed2010-06.epub"
         )


testLightspeedIssue :: Test
testLightspeedIssue = TestCase $
   assertNewName "Lightspeed Magazine, issue number in title" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [MetaCreator (Just "aut") (Just "Magazine, Lightspeed & Clark, Maggie & Valentine, Genevieve & Baxter, Stephen & Okorafor, Nnedi & Wilison, Daniel H. & Reed, Robert & Sedia, Ekaterina") "Lightspeed Magazine"]
         , metaTitles = [MetaTitle Nothing 
            "Lightspeed Magazine Issue 10"]
         }
      expected =
         ( "magLightspeedIssue"
         , "Lightspeed010.epub"
         )


testMagWeirdTales :: Test
testMagWeirdTales = TestCase $
   assertNewName "Weird Tales magazine" meta expected
   where
      meta = emptyMetadata
         { metaCreators =
            [ MetaCreator (Just "aut") Nothing "VanderMeer"
            , MetaCreator (Just "aut") Nothing "Ann"
            , MetaCreator (Just "aut") Nothing "Spinrad"
            , MetaCreator (Just "aut") Nothing "Norman"
            ]
         , metaTitles = [MetaTitle Nothing 
            "Weird Tales #350"]
         }
      expected =
         ( "magWeirdTales"
         , "WeirdTales350.epub"
         )
