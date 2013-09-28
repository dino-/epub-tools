-- Copyright: 2012-2013 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module EpubTools.Test.EpubName.Format
   ( formatTests
   )
   where

import Codec.Epub.Data.Metadata
import Codec.Epub.Data.Package
import Control.Monad.Error
import Test.HUnit ( Test (..), assertEqual )
import Test.HUnit.Base ( Assertion )

import EpubTools.EpubName.Format.Format
import EpubTools.EpubName.Format.Util
import EpubTools.EpubName.Opts


pkg2, pkg3 :: Package
pkg2 = Package "2.01" ""
pkg3 = Package "3.0" ""


formatTests :: Options -> [Formatter] -> Test
formatTests opts fs = TestLabel "Format" $ TestList $
   map (\f -> f ((Globals opts pkg2 emptyMetadata), fs))
      [ testAuthorMinimal
      , testAuthorOneName
      , testAuthorRole
      , testAuthorFileas
      , testAuthorFull
      , testMultiAutCreators
      , testMultiAutOneString
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
      , testPubYearEpub3Unwanted
      , testPubYearEpub2
      , testPubYearEpub2Any
      , testPubYearEpub2Orig
      , testPubYearEpub2Unwanted
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
      , testMagGalaxysEdge
      , testMagPlasmaFreq
      , testMagPlasmaFreqMonth
      , testAnthology
      ]


assertNewName :: Globals -> [Formatter] -> String
   -> (String, String) -> Assertion
assertNewName globals fs desc expected = do
   result <- runErrorT $ tryFormatting globals fs ""
   let actual = either (\em -> ("NO FORMATTER", em)) id result
   assertEqual desc expected actual


testAuthorMinimal :: (Globals, [Formatter]) -> Test
testAuthorMinimal (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs "minimal author" expected
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


testAuthorOneName :: (Globals, [Formatter]) -> Test
testAuthorOneName (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "author is a single word" expected
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


testAuthorRole :: (Globals, [Formatter]) -> Test
testAuthorRole (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs "author with role" expected
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


testAuthorFileas :: (Globals, [Formatter]) -> Test
testAuthorFileas (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs "author with file-as" expected
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


testAuthorFull :: (Globals, [Formatter]) -> Test
testAuthorFull (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs "author fully filled out" expected
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


testMultiAutCreators :: (Globals, [Formatter]) -> Test
testMultiAutCreators (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs "several authors" expected
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


testMultiAutOneString :: (Globals, [Formatter]) -> Test
testMultiAutOneString (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs "more than one author separated by & and/or and" expected
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


testNoAuthor :: (Globals, [Formatter]) -> Test
testNoAuthor (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs "no creator(s) at all" expected
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


testCreatorsNoAuthor :: (Globals, [Formatter]) -> Test
testCreatorsNoAuthor (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "creators, but no author(s) at all" expected
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


testCreatorsNoAuthorPubDate :: (Globals, [Formatter]) -> Test
testCreatorsNoAuthorPubDate (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "creators, but no author(s) at all, with pub date" expected
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


testCapsTitle :: (Globals, [Formatter]) -> Test
testCapsTitle (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs "title all caps" expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing Nothing "Greg Bear"]
         , metaTitles = [Title Nothing Nothing Nothing "EON"]
         }
      expected =
         ( "ordinary_book"
         , "BearGreg-Eon.epub"
         )


testColon :: (Globals, [Formatter]) -> Test
testColon (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "colon becomes underscore" expected
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


testBracketTitle :: (Globals, [Formatter]) -> Test
testBracketTitle (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "title with brackets" expected
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


testNoTitle :: (Globals, [Formatter]) -> Test
testNoTitle (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs "missing title" expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing
            Nothing "Nobody McCrankypants"]
         }
      expected =
         ( "NO FORMATTER"
         , " [ERROR No formatter found]"
         )


testAllPunctuation :: (Globals, [Formatter]) -> Test
testAllPunctuation (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs "big test of all punctuation" expected
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


testPubYearNoDatesPresent :: (Globals, [Formatter]) -> Test
testPubYearNoDatesPresent (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta} fs
      "book with no dates at all" expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing Nothing "Jim Jones"]
         , metaTitles = [Title Nothing Nothing Nothing "A Timeless Story"]
         }
      expected =
         ( "ordinary_book"
         , "JonesJim-ATimelessStory.epub"
         )


testPubYearEpub3 :: (Globals, [Formatter]) -> Test
testPubYearEpub3 (gs, fs) = TestCase $
   assertNewName gs { gPackage = pkg3, gMetadata = meta} fs
      "book with epub3 style (simple) date" expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing Nothing "Jim Jones"]
         , metaTitles = [Title Nothing Nothing Nothing "A Timeless Story"]
         , metaDates = [ Date Nothing "2003" ]
         }
      expected =
         ( "ordinary_book"
         , "JonesJim-ATimelessStory_2003.epub"
         )


testPubYearEpub3Unwanted :: (Globals, [Formatter]) -> Test
testPubYearEpub3Unwanted (gs, fs) = TestCase $
   assertNewName gs { gOpts = testOpts, gPackage = pkg3,
      gMetadata = meta } fs
      "epub3 book with a publication year but we don't want"
      expected
   where
      testOpts = (gOpts gs) { optPubYear = NoDate }
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing Nothing "Jim Jones"]
         , metaTitles = [Title Nothing Nothing Nothing "A Timeless Story"]
         , metaDates =
            [ Date Nothing "2001"
            , Date (Just "some date") "2000"
            ]
         }
      expected =
         ( "ordinary_book"
         , "JonesJim-ATimelessStory.epub"
         )


testPubYearEpub2 :: (Globals, [Formatter]) -> Test
testPubYearEpub2 (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta} fs
      "book with epub2 style (simple) date" expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing Nothing "Jim Jones"]
         , metaTitles = [Title Nothing Nothing Nothing "A Timeless Story"]
         , metaDates = [ Date Nothing "2003" ]
         }
      expected =
         ( "ordinary_book"
         , "JonesJim-ATimelessStory.epub"
         )


testPubYearEpub2Any :: (Globals, [Formatter]) -> Test
testPubYearEpub2Any (gs, fs) = TestCase $
   assertNewName gs { gOpts = testOpts, gMetadata = meta} fs
      "book with epub2 style (simple) date, any switch" expected
   where
      testOpts = (gOpts gs) { optPubYear = AnyDate }
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing Nothing "Jim Jones"]
         , metaTitles = [Title Nothing Nothing Nothing "A Timeless Story"]
         , metaDates = [ Date Nothing "2003" ]
         }
      expected =
         ( "ordinary_book"
         , "JonesJim-ATimelessStory_2003.epub"
         )


testPubYearEpub2Orig :: (Globals, [Formatter]) -> Test
testPubYearEpub2Orig (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "book with epub2 style dates, orig attr" expected
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


testPubYearEpub2Unwanted :: (Globals, [Formatter]) -> Test
testPubYearEpub2Unwanted (gs, fs) = TestCase $
   assertNewName gs { gOpts = testOpts, gMetadata = meta } fs
      "epub2 book with a publication year but we don't want"
      expected
   where
      testOpts = (gOpts gs) { optPubYear = NoDate }
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


testMagAeon :: (Globals, [Formatter]) -> Test
testMagAeon (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs "Aeon magazine" expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing Nothing "Aeon Authors"]
         , metaTitles = [Title Nothing Nothing Nothing "Aeon Eight"]
         }
      expected =
         ( "magAeon"
         , "AeonMagazine08.epub"
         )


testMagAEon :: (Globals, [Formatter]) -> Test
testMagAEon (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs "AEon magazine" expected
   where
      meta = emptyMetadata
         { metaCreators = [Creator Nothing Nothing Nothing "AEon Authors"]
         , metaTitles = [Title Nothing Nothing Nothing "Aeon Thirteen"]
         }
      expected =
         ( "magAeon"
         , "AeonMagazine13.epub"
         )


testMagApexLong :: (Globals, [Formatter]) -> Test
testMagApexLong (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Apex Magazine, older, long title" expected
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


testMagApexShort :: (Globals, [Formatter]) -> Test
testMagApexShort (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Apex Magazine, newer, short title" expected
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


testChallengingDestinyShort :: (Globals, [Formatter]) -> Test
testChallengingDestinyShort (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Challenging Destiny Magazine, short title" expected
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


testChallengingDestinyLong :: (Globals, [Formatter]) -> Test
testChallengingDestinyLong (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Challenging Destiny Magazine, long title" expected
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


testAnalog :: (Globals, [Formatter]) -> Test
testAnalog (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs "Analog" expected
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


testAsimovs :: (Globals, [Formatter]) -> Test
testAsimovs (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs "Asimovs" expected
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

testFantasyMagazine :: (Globals, [Formatter]) -> Test
testFantasyMagazine (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs "Fantasy Magazine" expected
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


testFsfShort :: (Globals, [Formatter]) -> Test
testFsfShort (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "FSF Magazine, short" expected
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


testFsfShortComma :: (Globals, [Formatter]) -> Test
testFsfShortComma (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "FSF Magazine, short, comma" expected
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


testFsfLong :: (Globals, [Formatter]) -> Test
testFsfLong (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "FSF Magazine, long" expected
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


testFsfAmpersand :: (Globals, [Formatter]) -> Test
testFsfAmpersand (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "FSF Magazine, ampersand" expected
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


testFsfAmpersandSpaces :: (Globals, [Formatter]) -> Test
testFsfAmpersandSpaces (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "FSF Magazine, ampersand, spaces" expected
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


testMagFutureOrbits :: (Globals, [Formatter]) -> Test
testMagFutureOrbits (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "testMagFutureOrbits" expected
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


testGudShort :: (Globals, [Formatter]) -> Test
testGudShort (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Gud Magazine, short" expected
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


testGudLong :: (Globals, [Formatter]) -> Test
testGudLong (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Gud Magazine, long" expected
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


testGudVeryLong :: (Globals, [Formatter]) -> Test
testGudVeryLong (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Gud Magazine, very long" expected
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


testInterzoneOldShort :: (Globals, [Formatter]) -> Test
testInterzoneOldShort (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Interzone Magazine, old style, short" expected
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


testInterzoneOldLong :: (Globals, [Formatter]) -> Test
testInterzoneOldLong (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Interzone Magazine, old style, long" expected
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


testInterzone :: (Globals, [Formatter]) -> Test
testInterzone (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Interzone Magazine, Smashwords style" expected
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


testNemesisShort :: (Globals, [Formatter]) -> Test
testNemesisShort (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Nemesis Magazine, short" expected
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


testNemesisLong :: (Globals, [Formatter]) -> Test
testNemesisLong (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Nemesis Magazine, long" expected
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


testMagSomethingWicked :: (Globals, [Formatter]) -> Test
testMagSomethingWicked (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Something Wicked Magazine" expected
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


testMagSomethingWickedMonth :: (Globals, [Formatter]) -> Test
testMagSomethingWickedMonth (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Something Wicked Magazine, month in title" expected
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


testSFBestOf :: (Globals, [Formatter]) -> Test
testSFBestOf (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Science Fiction: The Best of the Year" expected
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


testBestSF :: (Globals, [Formatter]) -> Test
testBestSF (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "The Best Science Fiction and Fantasy of the Year" expected
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


testYearsBest :: (Globals, [Formatter]) -> Test
testYearsBest (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "The Year's Best SF" expected
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


testMagBlackStatic :: (Globals, [Formatter]) -> Test
testMagBlackStatic (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Black Static Magazine" expected
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


testRageMachineMag :: (Globals, [Formatter]) -> Test
testRageMachineMag (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Rage Machine Magazine" expected
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


testEclipseMagWord :: (Globals, [Formatter]) -> Test
testEclipseMagWord (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Eclipse Magazine" expected
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


testEclipseMagNum :: (Globals, [Formatter]) -> Test
testEclipseMagNum (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Eclipse Magazine" expected
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


testBcs :: (Globals, [Formatter]) -> Test
testBcs (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Beneath Ceaseless Skies Magazine" expected
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


testBkpFileAs :: (Globals, [Formatter]) -> Test
testBkpFileAs (gs, fs) = TestCase $
   assertNewName gs { gOpts = testOpts, gMetadata = meta } fs
      "book publisher suffix requested and present in file-as"
      expected
   where
      testOpts = (gOpts gs) { optPublisher = True }
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


testBkpText :: (Globals, [Formatter]) -> Test
testBkpText (gs, fs) = TestCase $
   assertNewName gs { gOpts = testOpts, gMetadata = meta } fs
      "book publisher suffix requested and present in text"
      expected
   where
      testOpts = (gOpts gs) { optPublisher = True }
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


testBkpMissing :: (Globals, [Formatter]) -> Test
testBkpMissing (gs, fs) = TestCase $
   assertNewName gs { gOpts = testOpts, gMetadata = meta } fs
      "book publisher suffix requested and not present"
      expected
   where
      testOpts = (gOpts gs) { optPublisher = True }
      meta = emptyMetadata
         { metaCreators = [Creator (Just "aut") Nothing
            Nothing "Herman Melville"]
         , metaTitles = [Title Nothing Nothing Nothing "Moby Dick"]
         }
      expected =
         ( "ordinary_book"
         , "MelvilleHerman-MobyDick.epub"
         )


testMagUniverse :: (Globals, [Formatter]) -> Test
testMagUniverse (gs, fs) = TestCase $
   assertNewName gs { gOpts = testOpts, gMetadata = meta } fs
      "Jim Baen's Universe Magazine" expected
   where
      testOpts = (gOpts gs) { optPublisher = True }
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


testMagClarkesworld :: (Globals, [Formatter]) -> Test
testMagClarkesworld (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Clarkesworld Magazine" expected
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


testLightspeedDate :: (Globals, [Formatter]) -> Test
testLightspeedDate (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Lightspeed Magazine, date in title" expected
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


testLightspeedMagIssue :: (Globals, [Formatter]) -> Test
testLightspeedMagIssue (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Lightspeed Magazine, issue number in title" expected
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


testLightspeedIssue :: (Globals, [Formatter]) -> Test
testLightspeedIssue (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Lightspeed Magazine, issue number in title" expected
   where
      meta = emptyMetadata
         { metaTitles = [Title Nothing Nothing Nothing
            "Lightspeed Issue 33"]
         }
      expected =
         ( "magLightspeed_issue"
         , "Lightspeed033.epub"
         )


testMagWeirdTales :: (Globals, [Formatter]) -> Test
testMagWeirdTales (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Weird Tales magazine" expected
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


testMagGalaxysEdge :: (Globals, [Formatter]) -> Test
testMagGalaxysEdge (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Galaxy's Edge magazine" expected
   where
      meta = emptyMetadata
         { metaTitles = [Title Nothing Nothing Nothing
            "Galaxy's Edge Magazine: Issue 1 March 2013"]
         }
      expected =
         ( "magGenericWithIssue"
         , "GalaxysEdgeMagazine01.epub"
         )


testMagPlasmaFreq :: (Globals, [Formatter]) -> Test
testMagPlasmaFreq (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Plasma Frequency magazine" expected
   where
      meta = emptyMetadata
         { metaTitles = [Title Nothing Nothing Nothing
            "Plasma Frequency Magazine Issue 1"]
         }
      expected =
         ( "magGenericWithIssue"
         , "PlasmaFrequencyMagazine01.epub"
         )


testMagPlasmaFreqMonth :: (Globals, [Formatter]) -> Test
testMagPlasmaFreqMonth (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs
      "Plasma Frequency magazine, incl month/year in title" expected
   where
      meta = emptyMetadata
         { metaTitles = [Title Nothing Nothing Nothing
            "Plasma Frequency Magazine: Issue 8 October/November 2013"]
         }
      expected =
         ( "magGenericWithIssue"
         , "PlasmaFrequencyMagazine08.epub"
         )


testAnthology :: (Globals, [Formatter]) -> Test
testAnthology (gs, fs) = TestCase $
   assertNewName gs { gMetadata = meta } fs "anthology" expected
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
