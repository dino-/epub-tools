-- Copyright: 2012-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.Test.EpubName.Format
   ( formatTests
   )
   where

import Codec.Epub.Data.Metadata
import Control.Monad.Error
import Test.HUnit ( Test (..), assertEqual )
import Test.HUnit.Base ( Assertion )

import EpubTools.EpubName.Format.Format
import EpubTools.EpubName.Opts


formatTests :: Options -> [Formatter] -> Test
formatTests opts fs = TestLabel "Format" $ TestList $
   map (\f -> f (opts, fs))
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
      , testPubYearNoDatesPresent
      , testPubYearEpub3
      , testPubYearEpub2
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
      , testFsfShortComma
      , testFsfLong
      , testFsfAmpersand
      , testFsfAmpersandSpaces
      , testMagFutureOrbits
      , testGudShort
      , testGudLong
      , testGudVeryLong
      , testInterzoneOldShort
      , testInterzoneOldLong
      , testInterzone
      , testNemesisShort
      , testNemesisLong
      , testMagSomethingWicked
      , testMagSomethingWickedMonth
      , testSFBestOf
      , testBestSF
      , testYearsBest
      , testMagBlackStatic
      , testRageMachineMag
      , testEclipseMagWord
      , testEclipseMagNum
      , testBcs
      , testBkpFileAs
      , testBkpText
      , testBkpMissing
      , testMagUniverse
      , testMagClarkesworld
      , testLightspeedDate
      , testLightspeedMagIssue
      , testLightspeedIssue
      , testMagWeirdTales
      , testAnthology
      ]


assertNewName :: Options -> [Formatter] -> String -> Metadata
   -> (String, String) -> Assertion
assertNewName opts fs desc meta expected = do
   result <- runErrorT $ tryFormatting opts fs meta ""
   let actual = either (\em -> ("NO FORMATTER", em)) id result
   assertEqual desc expected actual


testAuthorMinimal :: (Options, [Formatter]) -> Test
testAuthorMinimal (opts, fs) = TestCase $
   assertNewName opts fs "minimal author" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing Nothing
            "Herman Melville"]
         , metaTitles = [Title Nothing Nothing Nothing "Moby Dick"]
         }
      expected =
         ( "ordinary_book"
         , "MelvilleHerman-MobyDick.epub"
         )


testAuthorOneName :: (Options, [Formatter]) -> Test
testAuthorOneName (opts, fs) = TestCase $
   assertNewName opts fs "author is a single word" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing Nothing
            "Melville"]
         , metaTitles = [Title Nothing Nothing Nothing "Moby Dick"]
         }
      expected =
         ( "ordinary_book"
         , "Melville-MobyDick.epub"
         )


testAuthorRole :: (Options, [Formatter]) -> Test
testAuthorRole (opts, fs) = TestCase $
   assertNewName opts fs "author with role" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator (Just "aut") Nothing
            Nothing "Herman Melville"]
         , metaTitles = [Title Nothing Nothing Nothing "Moby Dick"]
         }
      expected =
         ( "ordinary_book"
         , "MelvilleHerman-MobyDick.epub"
         )


testAuthorFileas :: (Options, [Formatter]) -> Test
testAuthorFileas (opts, fs) = TestCase $
   assertNewName opts fs "author with file-as" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing
            (Just "Melville, Herman")
            Nothing
            "Herman Melville"]
         , metaTitles = [Title Nothing Nothing Nothing "Moby Dick"]
         }
      expected =
         ( "ordinary_book"
         , "MelvilleHerman-MobyDick.epub"
         )


testAuthorFull :: (Options, [Formatter]) -> Test
testAuthorFull (opts, fs) = TestCase $
   assertNewName opts fs "author fully filled out" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator (Just "aut")
            (Just "Melville, Herman")
            Nothing
            "Herman Melville"]
         , metaTitles = [Title Nothing Nothing Nothing "Moby Dick"]
         }
      expected =
         ( "ordinary_book"
         , "MelvilleHerman-MobyDick.epub"
         )


testAuthorSeveral :: (Options, [Formatter]) -> Test
testAuthorSeveral (opts, fs) = TestCase $
   assertNewName opts fs "several authors" meta expected
   where
      meta = emptyMetadata
         { metaCreators =
            [ Creator (Just "aut") Nothing
               Nothing "James Patrick Kelly"
            , Creator (Just "aut") (Just "Kessel, John")
               Nothing "John Kessel"
            , Creator (Just "aut") Nothing
               Nothing "Jonathan Lethem"
            ]
         , metaTitles = [Title Nothing Nothing Nothing
            "Ninety Percent of Everything"]
         }
      expected =
         ( "ordinary_book"
         , "Kelly_Kessel_Lethem-NinetyPercentOfEverything.epub"
         )


testNoAuthor :: (Options, [Formatter]) -> Test
testNoAuthor (opts, fs) = TestCase $
   assertNewName opts fs "no creator(s) at all" meta expected
   where
      meta = emptyMetadata
         { metaCreators = []
         , metaTitles = [Title Nothing Nothing Nothing
            "Some Collection of Fine Stories, Volume 1"]
         }
      expected =
         ( "ordinary_book"
         , "SomeCollectionOfFineStoriesVolume1.epub"
         )


testCreatorsNoAuthor :: (Options, [Formatter]) -> Test
testCreatorsNoAuthor (opts, fs) = TestCase $
   assertNewName opts fs "creators, but no author(s) at all" meta expected
   where
      meta = emptyMetadata
         { metaCreators =
            [ Creator (Just "edt") Nothing
               Nothing "Graham Spindlewest"
            , Creator (Just "ill") Nothing
               Nothing "Eva Tunglewacker"
            ]
         , metaTitles = [Title Nothing Nothing Nothing
            "Some Collection of Fine Stories, Volume 1"]
         }
      expected =
         ( "ordinary_book"
         , "SomeCollectionOfFineStoriesVolume1.epub"
         )


testCreatorsNoAuthorPubDate :: (Options, [Formatter]) -> Test
testCreatorsNoAuthorPubDate (opts, fs) = TestCase $
   assertNewName opts fs "creators, but no author(s) at all, with pub date" 
      meta expected
   where
      meta = emptyMetadata
         { metaCreators =
            [ Creator (Just "edt") Nothing
               Nothing "Graham Spindlewest"
            , Creator (Just "ill") Nothing
               Nothing "Eva Tunglewacker"
            ]
         , metaTitles = [Title Nothing Nothing Nothing
            "Some Collection of Fine Stories, Volume 1"]
         , metaDates = [Date (Just "publication")
            "2008"]
         }
      expected =
         ( "ordinary_book"
         , "SomeCollectionOfFineStoriesVolume1_2008.epub"
         )


testSeveralAuthors :: (Options, [Formatter]) -> Test
testSeveralAuthors (opts, fs) = TestCase $
   assertNewName opts fs "more than one author separated by & and/or and" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing Nothing
            "Yvonne Q. Anderson & Eva Tunglewacker and Jefferson Milner"]
         , metaTitles = [Title Nothing Nothing Nothing "Big Trouble"]
         }
      expected =
         ( "ordinary_book"
         , "Anderson_Tunglewacker_Milner-BigTrouble.epub"
         )


testCapsTitle :: (Options, [Formatter]) -> Test
testCapsTitle (opts, fs) = TestCase $
   assertNewName opts fs "title all caps" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing Nothing "Greg Bear"]
         , metaTitles = [Title Nothing Nothing Nothing "EON"]
         }
      expected =
         ( "ordinary_book"
         , "BearGreg-Eon.epub"
         )


testColon :: (Options, [Formatter]) -> Test
testColon (opts, fs) = TestCase $
   assertNewName opts fs "colon becomes underscore" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing Nothing
            "Ed Howdershelt"]
         , metaTitles = [Title Nothing Nothing Nothing 
            "Book 1: 3rd World Products, Inc."]
         }
      expected =
         ( "ordinary_book"
         , "HowdersheltEd-Book1_3rdWorldProductsInc.epub"
         )


testBracketTitle :: (Options, [Formatter]) -> Test
testBracketTitle (opts, fs) = TestCase $
   assertNewName opts fs "title with brackets" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing Nothing "Mercedes Lackey"]
         , metaTitles = [Title Nothing Nothing Nothing
            "SKitty [Shipscat series #1]"]
         }
      expected =
         ( "ordinary_book"
         , "LackeyMercedes-Skitty_ShipscatSeries1.epub"
         )


testNoTitle :: (Options, [Formatter]) -> Test
testNoTitle (opts, fs) = TestCase $
   assertNewName opts fs "missing title" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing
            Nothing "Nobody McCrankypants"]
         }
      expected =
         ( "NO FORMATTER"
         , " [ERROR No formatter found]"
         )


testAllPunctuation :: (Options, [Formatter]) -> Test
testAllPunctuation (opts, fs) = TestCase $
   assertNewName opts fs "big test of all punctuation" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing
            Nothing "Dino Morelli"]
         , metaTitles = [Title Nothing Nothing Nothing
            "The *crazy*: Sand-box. Of Smedley's discontent/loathing, fear & Malnourishment? (Maybe not!); [Part #2]"]
         }
      expected =
         ( "ordinary_book"
         , "MorelliDino-TheCrazy_SandBoxOfSmedleysDiscontentLoathingFearAndMalnourishmentMaybeNot_Part2.epub"
         )


testPubYearNoDatesPresent :: (Options, [Formatter]) -> Test
testPubYearNoDatesPresent (opts, fs) = TestCase $
   assertNewName opts fs "book with no dates at all" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing Nothing "Jim Jones"]
         , metaTitles = [Title Nothing Nothing Nothing "A Timeless Story"]
         }
      expected =
         ( "ordinary_book"
         , "JonesJim-ATimelessStory.epub"
         )


testPubYearEpub3 :: (Options, [Formatter]) -> Test
testPubYearEpub3 (opts, fs) = TestCase $
   assertNewName opts fs "book with epub3 style (simple) date" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing Nothing "Jim Jones"]
         , metaTitles = [Title Nothing Nothing Nothing "A Timeless Story"]
         , metaDates =
            [ Date Nothing "2003"
            , Date (Just "some date") "2002"
            , Date Nothing "2001"
            ]
         }
      expected =
         ( "ordinary_book"
         , "JonesJim-ATimelessStory_2003.epub"
         )


testPubYearEpub2 :: (Options, [Formatter]) -> Test
testPubYearEpub2 (opts, fs) = TestCase $
   assertNewName opts fs "book with epub2 style dates" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing Nothing "Jim Jones"]
         , metaTitles = [Title Nothing Nothing Nothing "A Timeless Story"]
         , metaDates =
            [ Date Nothing "2001"
            , Date (Just "publication") "2003"
            , Date (Just "some date") "2000"
            , Date (Just "original-publication") "2002"
            ]
         }
      expected =
         ( "ordinary_book"
         , "JonesJim-ATimelessStory_2002.epub"
         )


testPubYearUnwanted :: (Options, [Formatter]) -> Test
testPubYearUnwanted (opts, fs) = TestCase $
   assertNewName testOpts fs
      "book with a publication year but we don't want"
      meta expected
   where
      testOpts = opts { optPubYear = NoDate }
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing Nothing "Jim Jones"]
         , metaTitles = [Title Nothing Nothing Nothing "A Timeless Story"]
         , metaDates =
            [ Date Nothing "2001"
            , Date (Just "publication") "2003"
            , Date (Just "some date") "2000"
            , Date (Just "original-publication") "2002"
            ]
         }
      expected =
         ( "ordinary_book"
         , "JonesJim-ATimelessStory.epub"
         )


testMagAeon :: (Options, [Formatter]) -> Test
testMagAeon (opts, fs) = TestCase $
   assertNewName opts fs "Aeon magazine" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing Nothing "Aeon Authors"]
         , metaTitles = [Title Nothing Nothing Nothing "Aeon Eight"]
         }
      expected =
         ( "magAeon"
         , "AeonMagazine08.epub"
         )


testMagAEon :: (Options, [Formatter]) -> Test
testMagAEon (opts, fs) = TestCase $
   assertNewName opts fs "AEon magazine" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing Nothing "AEon Authors"]
         , metaTitles = [Title Nothing Nothing Nothing "Aeon Thirteen"]
         }
      expected =
         ( "magAeon"
         , "AeonMagazine13.epub"
         )


testMagApexLong :: (Options, [Formatter]) -> Test
testMagApexLong (opts, fs) = TestCase $
   assertNewName opts fs "Apex Magazine, older, long title" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator (Just "aut") Nothing
            Nothing "Apex Authors"]
         , metaTitles = [Title Nothing Nothing Nothing
            "Apex Science Fiction and Horror Digest #9"]
         }
      expected =
         ( "magApex"
         , "ApexMagazine009.epub"
         )


testMagApexShort :: (Options, [Formatter]) -> Test
testMagApexShort (opts, fs) = TestCase $
   assertNewName opts fs "Apex Magazine, newer, short title" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator (Just "aut") Nothing
            Nothing "Apex Authors"]
         , metaTitles = [Title Nothing Nothing Nothing
            "Apex Magazine Issue 17"]
         }
      expected =
         ( "magApex"
         , "ApexMagazine017.epub"
         )


testChallengingDestinyShort :: (Options, [Formatter]) -> Test
testChallengingDestinyShort (opts, fs) = TestCase $
   assertNewName opts fs
      "Challenging Destiny Magazine, short title" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing
            Nothing "Crystalline Sphere Authors"]
         , metaTitles = [Title Nothing Nothing Nothing
            "Challenging Destiny #23"]
         }
      expected =
         ( "magChallengingDestiny"
         , "ChallengingDestinyMagazine023.epub"
         )


testChallengingDestinyLong :: (Options, [Formatter]) -> Test
testChallengingDestinyLong (opts, fs) = TestCase $
   assertNewName opts fs
      "Challenging Destiny Magazine, long title" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing 
            Nothing "Crystalline Sphere Authors"]
         , metaTitles = [Title Nothing Nothing Nothing 
            "Challenging Destiny #24: August 2007"]
         }
      expected =
         ( "magChallengingDestiny"
         , "ChallengingDestinyMagazine024.epub"
         )


testAnalog :: (Options, [Formatter]) -> Test
testAnalog (opts, fs) = TestCase $
   assertNewName opts fs "Analog" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing
            Nothing "Dell Magazine Authors"]
         , metaTitles = [Title Nothing Nothing Nothing
            "Analog SFF, July-August 2003"]
         }
      expected =
         ( "magAnalog"
         , "AnalogSF2003-07_08.epub"
         )


testAsimovs :: (Options, [Formatter]) -> Test
testAsimovs (opts, fs) = TestCase $
   assertNewName opts fs "Asimovs" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing
            Nothing "Dell Magazine Authors"]
         , metaTitles = [Title Nothing Nothing Nothing
            "Asimov's SF, August 2003"]
         }
      expected =
         ( "magAnalog"
         , "AsimovsSF2003-08.epub"
         )

testFantasyMagazine :: (Options, [Formatter]) -> Test
testFantasyMagazine (opts, fs) = TestCase $
   assertNewName opts fs "Fantasy Magazine" meta expected
   where
      meta = emptyMetadata
         { metaCreators =
            [ Creator (Just "aut") (Just "Howard, Kat & Tanzer, Molly & Beagle, Peter S. & Howard, Jonathan L. & Valentine, Genevieve & Vaughn, Carrie & Pilinovsky, Helen") Nothing "Kat Howard"
            , Creator (Just "aut") Nothing Nothing "Molly Tanzer"
            , Creator (Just "aut") Nothing Nothing "Peter S. Beagle"
            , Creator (Just "aut") Nothing Nothing "Jonathan L. Howard"
            , Creator (Just "aut") Nothing Nothing "Genevieve Valentine"
            , Creator (Just "aut") Nothing Nothing "Carrie Vaughn"
            , Creator (Just "aut") Nothing Nothing "Helen Pilinovsky"
            ]
         , metaTitles = [Title Nothing Nothing Nothing
            "Fantasy Magazine Issue 49"]
         }
      expected =
         ( "magFantasyMag"
         , "FantasyMagazine049.epub"
         )


testFsfShort :: (Options, [Formatter]) -> Test
testFsfShort (opts, fs) = TestCase $
   assertNewName opts fs "FSF Magazine, short" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing
            Nothing "Spilogale Authors"]
         , metaTitles = [Title Nothing Nothing Nothing "FSF Oct/Nov 2003"]
         }
      expected =
         ( "magFsf"
         , "FantasyScienceFiction2003-10_11.epub"
         )


testFsfShortComma :: (Options, [Formatter]) -> Test
testFsfShortComma (opts, fs) = TestCase $
   assertNewName opts fs "FSF Magazine, short, comma" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing
            Nothing "Spilogale Authors"]
         , metaTitles = [Title Nothing Nothing Nothing "FSF, April 2008"]
         }
      expected =
         ( "magFsf"
         , "FantasyScienceFiction2008-04.epub"
         )


testFsfLong :: (Options, [Formatter]) -> Test
testFsfLong (opts, fs) = TestCase $
   assertNewName opts fs "FSF Magazine, long" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing 
            Nothing "Spilogale Authors"]
         , metaTitles = [Title Nothing Nothing Nothing 
            "FSF Magazine, April 2006"]
         }
      expected =
         ( "magFsf"
         , "FantasyScienceFiction2006-04.epub"
         )


testFsfAmpersand :: (Options, [Formatter]) -> Test
testFsfAmpersand (opts, fs) = TestCase $
   assertNewName opts fs "FSF Magazine, ampersand" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing
            Nothing "Spilogale"]
         , metaTitles = [Title Nothing Nothing Nothing "F&SF, Apr 2004"]
         }
      expected =
         ( "magFsf"
         , "FantasyScienceFiction2004-04.epub"
         )


testFsfAmpersandSpaces :: (Options, [Formatter]) -> Test
testFsfAmpersandSpaces (opts, fs) = TestCase $
   assertNewName opts fs "FSF Magazine, ampersand, spaces" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing
            Nothing "Spilogale"]
         , metaTitles = [Title Nothing Nothing Nothing "F & SF Dec 2003"]
         }
      expected =
         ( "magFsf"
         , "FantasyScienceFiction2003-12.epub"
         )


testMagFutureOrbits :: (Options, [Formatter]) -> Test
testMagFutureOrbits (opts, fs) = TestCase $
   assertNewName opts fs "testMagFutureOrbits" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing 
            Nothing "Vander Neut Publications, L.L.C."]
         , metaTitles = [Title Nothing Nothing Nothing 
            "Future Orbits Issue 5, June/July 2002"]
         }
      expected =
         ( "magFutureOrbits"
         , "FutureOrbitsMagazine05_2002-06_07.epub"
         )


testGudShort :: (Options, [Formatter]) -> Test
testGudShort (opts, fs) = TestCase $
   assertNewName opts fs "Gud Magazine, short" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing 
            Nothing "GUD Magazine Authors"]
         , metaTitles = [Title Nothing Nothing Nothing 
            "GUD Magazine Issue 0 :: Spring 2007"]
         }
      expected =
         ( "magGud"
         , "GUDMagazine00.epub"
         )


testGudLong :: (Options, [Formatter]) -> Test
testGudLong (opts, fs) = TestCase $
   assertNewName opts fs "Gud Magazine, long" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing 
            Nothing "GUD Magazine Authors, Jeff Somers, Jeremy Shipp"]
         , metaTitles = [Title Nothing Nothing Nothing 
            "GUD Magazine Issue 2 :: Spring 2008"]
         }
      expected =
         ( "magGud"
         , "GUDMagazine02.epub"
         )


testGudVeryLong :: (Options, [Formatter]) -> Test
testGudVeryLong (opts, fs) = TestCase $
   assertNewName opts fs "Gud Magazine, very long" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing 
            Nothing "GUD Magazine Authors"]
         , metaTitles = [Title Nothing Nothing Nothing 
            "Greatest Uncommon Denominator Magazine Issue 4 :: Spring 2009"]
         }
      expected =
         ( "magGud"
         , "GUDMagazine04.epub"
         )


testInterzoneOldShort :: (Options, [Formatter]) -> Test
testInterzoneOldShort (opts, fs) = TestCase $
   assertNewName opts fs "Interzone Magazine, old style, short" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing 
            Nothing "TTA Press Authors"]
         , metaTitles = [Title Nothing Nothing Nothing 
            "Interzone SFF #212"]
         }
      expected =
         ( "magInterzone_old"
         , "InterzoneSFF212.epub"
         )


testInterzoneOldLong :: (Options, [Formatter]) -> Test
testInterzoneOldLong (opts, fs) = TestCase $
   assertNewName opts fs "Interzone Magazine, old style, long" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing 
            Nothing "TTA Press Authors"]
         , metaTitles = [Title Nothing Nothing Nothing 
            "Interzone Science Fiction and Fantasy Magazine #216"]
         }
      expected =
         ( "magInterzone_old"
         , "InterzoneSFF216.epub"
         )


testInterzone :: (Options, [Formatter]) -> Test
testInterzone (opts, fs) = TestCase $
   assertNewName opts fs "Interzone Magazine, Smashwords style" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing 
            Nothing "TTA Press"]
         , metaTitles = [Title Nothing Nothing Nothing 
            "Interzone 233 Mar - Apr 2011"]
         }
      expected =
         ( "magInterzone"
         , "InterzoneSFF233.epub"
         )


testNemesisShort :: (Options, [Formatter]) -> Test
testNemesisShort (opts, fs) = TestCase $
   assertNewName opts fs "Nemesis Magazine, short" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing 
            Nothing "Stephen Adams"]
         , metaTitles = [Title Nothing Nothing Nothing
            "Nemesis Magazine #2"]
         }
      expected =
         ( "magNemesis"
         , "NemesisMag002.epub"
         )


testNemesisLong :: (Options, [Formatter]) -> Test
testNemesisLong (opts, fs) = TestCase $
   assertNewName opts fs "Nemesis Magazine, long" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing 
            Nothing "Stephen Adams"]
         , metaTitles = [Title Nothing Nothing Nothing
            "Nemesis Magazine #7: Featuring Victory Rose in Death Stalks the Ruins"]
         }
      expected =
         ( "magNemesis"
         , "NemesisMag007.epub"
         )


testMagSomethingWicked :: (Options, [Formatter]) -> Test
testMagSomethingWicked (opts, fs) = TestCase $
   assertNewName opts fs "Something Wicked Magazine" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing Nothing
            "Something Wicked Authors"]
         , metaTitles = [Title Nothing Nothing Nothing
            "Something Wicked SF and Horror Magazine #5"]
         }
      expected =
         ( "magSomethingWicked"
         , "SomethingWicked005.epub"
         )


testMagSomethingWickedMonth :: (Options, [Formatter]) -> Test
testMagSomethingWickedMonth (opts, fs) = TestCase $
   assertNewName opts fs "Something Wicked Magazine, month in title" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator (Just "aut")
            (Just "Authors, Something Wicked")
            Nothing
            "Something Wicked Authors"]
         , metaTitles = [Title Nothing Nothing Nothing
            "Something Wicked #14 (October 2011)"]
         }
      expected =
         ( "magSomethingWicked"
         , "SomethingWicked014.epub"
         )


testSFBestOf :: (Options, [Formatter]) -> Test
testSFBestOf (opts, fs) = TestCase $
   assertNewName opts fs "Science Fiction: The Best of the Year" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing 
            Nothing "Rich Horton"]
         , metaTitles = [Title Nothing Nothing Nothing
            "Science Fiction: The Best of the Year, 2007 Edition"]
         , metaSubjects = ["anthology"]
         }
      expected =
         ( "anthology"
         , "ScienceFiction_TheBestOfTheYear2007Edition.epub"
         )


testBestSF :: (Options, [Formatter]) -> Test
testBestSF (opts, fs) = TestCase $
   assertNewName opts fs "The Best Science Fiction and Fantasy of the Year"
      meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator (Just "aut")
            (Just "Strahan, Jonathan")
            Nothing
            "Jonathan Strahan"]
         , metaTitles = [Title Nothing Nothing Nothing
            "The Best Science Fiction and Fantasy of the Year: Volume 2"]
         , metaSubjects = ["anthology"]
         }
      expected =
         ( "anthology"
         , "TheBestScienceFictionAndFantasyOfTheYear_Volume2.epub"
         )


testYearsBest :: (Options, [Formatter]) -> Test
testYearsBest (opts, fs) = TestCase $
   assertNewName opts fs "The Year's Best SF" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator (Just "aut") Nothing Nothing
            "Rich Horton, Michael Swanwick, Karen Joy Fowler"]
         , metaTitles = [Title Nothing Nothing Nothing
            "The Year's Best Science Fiction: 2008 Edition"]
         , metaSubjects = ["anthology"]
         }
      expected =
         ( "anthology"
         , "TheYearsBestScienceFiction_2008Edition.epub"
         )


testMagBlackStatic :: (Options, [Formatter]) -> Test
testMagBlackStatic (opts, fs) = TestCase $
   assertNewName opts fs "Black Static Magazine" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing 
            Nothing "TTA Press Authors"]
         , metaTitles = [Title Nothing Nothing Nothing
            "Black Static Horror Magazine #5"]
         }
      expected =
         ( "magBlackStatic"
         , "BlackStaticHorrorMagazine05.epub"
         )


testRageMachineMag :: (Options, [Formatter]) -> Test
testRageMachineMag (opts, fs) = TestCase $
   assertNewName opts fs "Rage Machine Magazine" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator (Just "edt") Nothing 
            Nothing "G. W. Thomas"]
         , metaTitles = [Title Nothing Nothing Nothing
            "Rage Machine Magazine #1--December 2005"]
         }
      expected =
         ( "magRageMachine"
         , "RageMachineMagazine1_2005-12.epub"
         )


testEclipseMagWord :: (Options, [Formatter]) -> Test
testEclipseMagWord (opts, fs) = TestCase $
   assertNewName opts fs "Eclipse Magazine" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator (Just "aut")
            (Just "Strahan, Jonathan")
            Nothing
            "Jonathan Strahan"]
         , metaTitles = [Title Nothing Nothing Nothing
            "Eclipse One"]
         }
      expected =
         ( "magEclipse_word"
         , "Eclipse01.epub"
         )


testEclipseMagNum :: (Options, [Formatter]) -> Test
testEclipseMagNum (opts, fs) = TestCase $
   assertNewName opts fs "Eclipse Magazine" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator (Just "aut")
            (Just "Strahan, Jonathan")
            Nothing
            "Jonathan Strahan"]
         , metaTitles = [Title Nothing Nothing Nothing
            "Eclipse 4: New Science Fiction and Fantasy"]
         }
      expected =
         ( "magEclipse_num"
         , "Eclipse04.epub"
         )


testBcs :: (Options, [Formatter]) -> Test
testBcs (opts, fs) = TestCase $
   assertNewName opts fs "Beneath Ceaseless Skies Magazine" meta expected
   where
      meta = emptyMetadata
         { metaCreators = 
            [ Creator (Just "aut")
               (Just "Tidwell, Erin A.")
               Nothing
               "Hoover, Kenneth Mark"
            , Creator (Just "aut")
               (Just "Tidwell, Erin A.")
               Nothing
               "Tidwell, Erin A."
            ]
         , metaTitles = [Title Nothing Nothing Nothing
            "Beneath Ceaseless Skies #32"]
         }
      expected =
         ( "magBcs"
         , "BeneathCeaselessSkies_Issue032.epub"
         )


testBkpFileAs :: (Options, [Formatter]) -> Test
testBkpFileAs (opts, fs) = TestCase $
   assertNewName testOpts fs
      "book publisher suffix requested and present in file-as"
      meta expected
   where
      testOpts = opts { optPublisher = True }
      meta = emptyMetadata
         { metaCreators = [Creator (Just "aut") Nothing
            Nothing "Herman Melville"]
         , metaContributors = [Creator (Just "bkp") (Just "acme") 
            Nothing "Acme Publishing, Inc."]
         , metaTitles = [Title Nothing Nothing Nothing "Moby Dick"]
         }
      expected =
         ( "ordinary_book"
         , "MelvilleHerman-MobyDick_acme.epub"
         )


testBkpText :: (Options, [Formatter]) -> Test
testBkpText (opts, fs) = TestCase $
   assertNewName testOpts fs
      "book publisher suffix requested and present in text"
      meta expected
   where
      testOpts = opts { optPublisher = True }
      meta = emptyMetadata
         { metaCreators = [Creator (Just "aut") Nothing
            Nothing "Herman Melville"]
         , metaContributors = [Creator (Just "bkp") Nothing
            Nothing "Acme Publishing, Inc."]
         , metaTitles = [Title Nothing Nothing Nothing "Moby Dick"]
         }
      expected =
         ( "ordinary_book"
         , "MelvilleHerman-MobyDick.epub"
         )


testBkpMissing :: (Options, [Formatter]) -> Test
testBkpMissing (opts, fs) = TestCase $
   assertNewName testOpts fs
      "book publisher suffix requested and not present"
      meta expected
   where
      testOpts = opts { optPublisher = True }
      meta = emptyMetadata
         { metaCreators = [Creator (Just "aut") Nothing
            Nothing "Herman Melville"]
         , metaTitles = [Title Nothing Nothing Nothing "Moby Dick"]
         }
      expected =
         ( "ordinary_book"
         , "MelvilleHerman-MobyDick.epub"
         )


testMagUniverse :: (Options, [Formatter]) -> Test
testMagUniverse (opts, fs) = TestCase $
   assertNewName testOpts fs
      "Jim Baen's Universe Magazine"
      meta expected
   where
      testOpts = opts { optPublisher = True }
      meta = emptyMetadata
         { metaCreators = [Creator (Just "aut") Nothing
            Nothing "Jim Baen's Universe"]
         , metaTitles = [Title Nothing Nothing Nothing
            "Jim Baen's Universe-Vol 4 Num 6"]
         }
      expected =
         ( "magUniverse"
         , "JimBaensUniverseVol04Num06.epub"
         )


testMagClarkesworld :: (Options, [Formatter]) -> Test
testMagClarkesworld (opts, fs) = TestCase $
   assertNewName opts fs "Clarkesworld Magazine" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator (Just "aut")
            (Just "Kowal, Mary Robinette") Nothing
            "Mary Robinette Kowal"]
         , metaTitles = [Title Nothing Nothing Nothing
            "Clarkesworld Magazine - Issue 21"]
         }
      expected =
         ( "magClarkesworld"
         , "Clarkesworld021.epub"
         )


testLightspeedDate :: (Options, [Formatter]) -> Test
testLightspeedDate (opts, fs) = TestCase $
   assertNewName opts fs "Lightspeed Magazine, date in title" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator (Just "aut") Nothing Nothing
            "Edited by John Joseph Adams"]
         , metaTitles = [Title Nothing Nothing Nothing
            "Lightspeed Magazine, June 2010"]
         }
      expected =
         ( "magLightspeed_date"
         , "Lightspeed2010-06.epub"
         )


testLightspeedMagIssue :: (Options, [Formatter]) -> Test
testLightspeedMagIssue (opts, fs) = TestCase $
   assertNewName opts fs
      "Lightspeed Magazine, issue number in title" meta expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator (Just "aut") (Just "Magazine, Lightspeed & Clark, Maggie & Valentine, Genevieve & Baxter, Stephen & Okorafor, Nnedi & Wilison, Daniel H. & Reed, Robert & Sedia, Ekaterina") Nothing "Lightspeed Magazine"]
         , metaTitles = [Title Nothing Nothing Nothing
            "Lightspeed Magazine Issue 10"]
         }
      expected =
         ( "magLightspeed_issue"
         , "Lightspeed010.epub"
         )


testLightspeedIssue :: (Options, [Formatter]) -> Test
testLightspeedIssue (opts, fs) = TestCase $
   assertNewName opts fs
      "Lightspeed Magazine, issue number in title" meta expected
   where
      meta = emptyMetadata
         { metaTitles = [Title Nothing Nothing Nothing
            "Lightspeed Issue 33"]
         }
      expected =
         ( "magLightspeed_issue"
         , "Lightspeed033.epub"
         )


testMagWeirdTales :: (Options, [Formatter]) -> Test
testMagWeirdTales (opts, fs) = TestCase $
   assertNewName opts fs "Weird Tales magazine" meta expected
   where
      meta = emptyMetadata
         { metaCreators =
            [ Creator (Just "aut") Nothing Nothing "VanderMeer"
            , Creator (Just "aut") Nothing Nothing "Ann"
            , Creator (Just "aut") Nothing Nothing "Spinrad"
            , Creator (Just "aut") Nothing Nothing "Norman"
            ]
         , metaTitles = [Title Nothing Nothing Nothing
            "Weird Tales #350"]
         }
      expected =
         ( "magWeirdTales"
         , "WeirdTales350.epub"
         )


testAnthology :: (Options, [Formatter]) -> Test
testAnthology (opts, fs) = TestCase $
   assertNewName opts fs "anthology" meta expected
   where
      meta = emptyMetadata
         { metaTitles = [Title Nothing Nothing Nothing "Creepy Secrets"]
         , metaCreators =
            [ Creator (Just "aut") (Just "Snively, Mortimer") Nothing
               "Mortimer Snively"
            , Creator (Just "aut") (Just "Baxter, Joanne") Nothing
               "Joanne Baxter"
            ]
         , metaDates = [Date (Just "publication") "2010-11-01"]
         , metaSubjects = ["fantasy and horror anthology"]
         }
      expected =
         ( "anthology"
         , "CreepySecrets_2010.epub"
         )
