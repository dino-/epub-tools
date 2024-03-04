module EpubTools.Test.EpubName.Format
  ( formatTests
  )
  where

import Codec.Epub.Data.Metadata (Creator (..), DateEvent (..), DateValue (..),
  Metadata (..), Title (..), emptyMetadata)
import Codec.Epub.Data.Package (Package (..))
import Control.Monad.Except (runExceptT)
import qualified Data.Map.Strict as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@=?), testCase)

import EpubTools.EpubName.Common
  ( Options (includePublisher, pubYear)
  , PublisherSwitch (..)
  , PubYear (NoDate, NoModified)
  )
import EpubTools.EpubName.Format.Format (Formatter, tryFormatting)
import EpubTools.EpubName.Format.Util (Globals (..))


formatTests :: Options -> [Formatter] -> TestTree
formatTests opts fs = testGroup "Format" $
  map (\f -> f ((Globals opts (Package "3.3" "") emptyMetadata), fs))
    [ testAuthorMinimal
    , testAuthorOneName
    , testAuthorRole
    , testAuthorFileas
    , testAuthorFileasParens
    , testAuthorFull
    , testAuthorMiddle
    , testMultiAutCreators
    , testMultiAutOneString
    , testNoAuthor
    , testColon
    , testCreatorsNoAuthor
    , testCreatorsNoAuthorPubDate
    , testCreatorLastFirst
    , testTitleCaps
    , testTitleBracket
    , testTitleNone
    , testTitleMultiline
    , testTitleHyphen
    , testTitleHyphenDates
    , testTitleRomanNum
    , testAllPunctuation
    , testPubYearNoDatesPresent
    , testPubYearAnyIssued
    , testPubYearAnyCreated
    , testPubYearAnyDate
    , testPubYearAnyEpub
    , testPubYearAnyModified
    , testPubYearAnyAllPresent
    , testPubYearNoModified
    , testPubYearNoDate
    , testMagAeon
    , testMagAEon
    , testMagApexPound
    , testMagApexIssue
    , testMagApexLong
    , testChallengingDestinyShort
    , testChallengingDestinyLong
    , testAnalogSingle
    , testAnalogDouble
    , testAsimovs
    , testFantasyMagazine
    , testFsfShort
    , testFsfShortComma
    , testFsfLong
    , testFsfAmpersand
    , testFsfAmpersandSpaces
    , testFsfVeryLong
    , testMagFutureOrbits
    , testGudShort
    , testGudLong
    , testGudVeryLong
    , testInterzone
    , testInterzoneCaps
    , testInterzoneOldLong
    , testInterzoneOldShort
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
    , testLightspeedMagIssueDate
    , testMagWeirdTales
    , testMagGalaxysEdge
    , testMagPlasmaFreq
    , testMagPlasmaFreqMonth
    , testMagPunchinello1800s
    , testMagGenericVolNo3
    , testMagSubjWithIssue
    , testWomensInstituteLib
    , testAnthologyDate
    , testAnthology
    , testMagLunaStation
    ]


assertNewName :: Globals -> [Formatter] -> String
  -> (String, FilePath) -> TestTree
assertNewName globals fs desc expected = testCase desc $ do
  result <- runExceptT $ tryFormatting globals fs ""
  let actual = either (\em -> ("NO FORMATTER", em)) id result
  expected @=? actual


testAuthorMinimal :: (Globals, [Formatter]) -> TestTree
testAuthorMinimal (gs, fs) =
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


testAuthorOneName :: (Globals, [Formatter]) -> TestTree
testAuthorOneName (gs, fs) =
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


testAuthorRole :: (Globals, [Formatter]) -> TestTree
testAuthorRole (gs, fs) =
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


testAuthorFileas :: (Globals, [Formatter]) -> TestTree
testAuthorFileas (gs, fs) =
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


testAuthorFileasParens :: (Globals, [Formatter]) -> TestTree
testAuthorFileasParens (gs, fs) =
  assertNewName gs { gMetadata = meta } fs
    "author with file-as and pesky parenthesized full names" expected
  where
    meta = emptyMetadata
      { metaCreators = [Creator Nothing
        (Just "Fletcher, J. S. (Joseph Smith)")
        Nothing
        "J. S. Fletcher"]
      , metaTitles = [Title Nothing Nothing Nothing "The Middle of Things"]
      }
    expected =
      ( "ordinary_book"
      , "FletcherJS-TheMiddleOfThings.epub"
      )


testAuthorFull :: (Globals, [Formatter]) -> TestTree
testAuthorFull (gs, fs) =
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


testAuthorMiddle :: (Globals, [Formatter]) -> TestTree
testAuthorMiddle (gs, fs) =
  assertNewName gs { gMetadata = meta } fs "author with middle-name" expected
  where
    meta = emptyMetadata
      { metaCreators = [Creator (Just "aut")
        (Just "Wallace, David Foster")
        Nothing
        "David Foster Wallace"]
      , metaTitles = [Title Nothing Nothing Nothing "Infinite Jest"]
      }
    expected =
      ( "ordinary_book"
      , "WallaceDavidFoster-InfiniteJest.epub"
      )


testMultiAutCreators :: (Globals, [Formatter]) -> TestTree
testMultiAutCreators (gs, fs) =
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


testMultiAutOneString :: (Globals, [Formatter]) -> TestTree
testMultiAutOneString (gs, fs) =
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


testNoAuthor :: (Globals, [Formatter]) -> TestTree
testNoAuthor (gs, fs) =
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


testCreatorsNoAuthor :: (Globals, [Formatter]) -> TestTree
testCreatorsNoAuthor (gs, fs) =
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


testColon :: (Globals, [Formatter]) -> TestTree
testColon (gs, fs) =
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


testCreatorsNoAuthorPubDate :: (Globals, [Formatter]) -> TestTree
testCreatorsNoAuthorPubDate (gs, fs) =
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
      , metaDates = Map.fromList [(Created, DateValue "2008")]
      }
    expected =
      ( "ordinary_book"
      , "SomeCollectionOfFineStoriesVolume1_2008.epub"
      )


testCreatorLastFirst :: (Globals, [Formatter]) -> TestTree
testCreatorLastFirst (gs, fs) =
  assertNewName gs { gMetadata = meta } fs
    "creator text (not file-as) is last-name-first" expected
  where
    meta = emptyMetadata
      { metaCreators =
        [ Creator Nothing Nothing
          Nothing "Spindlewest, Graham"
        ]
      , metaTitles = [Title Nothing Nothing Nothing
        "A Midnight's Horror Story"]
      }
    expected =
      ( "ordinary_book"
      , "SpindlewestGraham-AMidnightsHorrorStory.epub"
      )


testTitleCaps :: (Globals, [Formatter]) -> TestTree
testTitleCaps (gs, fs) =
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


testTitleBracket :: (Globals, [Formatter]) -> TestTree
testTitleBracket (gs, fs) =
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


testTitleNone :: (Globals, [Formatter]) -> TestTree
testTitleNone (gs, fs) =
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


testTitleMultiline :: (Globals, [Formatter]) -> TestTree
testTitleMultiline (gs, fs) =
  assertNewName gs { gMetadata = meta } fs
    "multiline title" expected
  where
    meta = emptyMetadata
      { metaCreators = [Creator
        Nothing
        (Just "Castleman, Virginia Carter")
        Nothing
        "Virginia Carter Castleman"]
      , metaTitles = [Title Nothing Nothing Nothing
        "Pocahontas.\nA Poem"]
      }
    expected =
      ( "ordinary_book"
      , "CastlemanVirginiaCarter-Pocahontas_APoem.epub"
      )


testTitleHyphen :: (Globals, [Formatter]) -> TestTree
testTitleHyphen (gs, fs) =
  assertNewName gs { gMetadata = meta } fs
    "title with hyphen" expected
  where
    meta = emptyMetadata
      { metaCreators = [Creator
        Nothing
        (Just "Spinoza, Benedictus de")
        Nothing
        "Benedictus de Spinoza"]
      , metaTitles = [Title Nothing Nothing Nothing
        "Theologico-Political Treatise"]
      }
    expected =
      ( "ordinary_book"
      , "SpinozaBenedictusDe-Theologico-PoliticalTreatise.epub"
      )


testTitleHyphenDates :: (Globals, [Formatter]) -> TestTree
testTitleHyphenDates (gs, fs) =
  assertNewName gs { gMetadata = meta } fs
    "title with hyphen separating dates" expected
  where
    meta = emptyMetadata
      { metaCreators = [Creator
        Nothing
        (Just "Anonymous")
        Nothing
        "Anonymous"]
      , metaTitles = [Title Nothing Nothing Nothing
        "Queen Victoria\nStory of Her Life and Reign, 1819-1901"]
      }
    expected =
      ( "ordinary_book"
      , "Anonymous-QueenVictoria_StoryOfHerLifeAndReign1819-1901.epub"
      )

testTitleRomanNum :: (Globals, [Formatter]) -> TestTree
testTitleRomanNum (gs, fs) =
  assertNewName gs { gMetadata = meta } fs
    "title with Roman numerals" expected
  where
    meta = emptyMetadata
      { metaCreators = [Creator
        Nothing
        (Just "Hauptmann, Gerhart")
        Nothing
        "Gerhart Hauptmann"]
      , metaTitles = [Title Nothing Nothing Nothing
        "The Dramatic Works of Gerhart Hauptmann\nVolume III"]
      }
    expected =
      ( "ordinary_book"
      , "HauptmannGerhart-TheDramaticWorksOfGerhartHauptmann_VolumeIII.epub"
      )


testAllPunctuation :: (Globals, [Formatter]) -> TestTree
testAllPunctuation (gs, fs) =
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
      , "MorelliDino-TheCrazy_Sand-BoxOfSmedleysDiscontentLoathingFearAndMalnourishmentMaybeNot_Part2.epub"
      )


testPubYearNoDatesPresent :: (Globals, [Formatter]) -> TestTree
testPubYearNoDatesPresent (gs, fs) =
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


testPubYearAnyIssued :: (Globals, [Formatter]) -> TestTree
testPubYearAnyIssued (gs, fs) =
  assertNewName gs { gMetadata = meta } fs
    "book with only issued date" expected
  where
    meta = emptyMetadata
      { metaCreators = [Creator Nothing Nothing Nothing "Jim Jones"]
      , metaTitles = [Title Nothing Nothing Nothing "A Timeless Story"]
      , metaDates = Map.fromList [(Issued, DateValue "2003")]
      }
    expected =
      ( "ordinary_book"
      , "JonesJim-ATimelessStory_2003.epub"
      )


testPubYearAnyCreated :: (Globals, [Formatter]) -> TestTree
testPubYearAnyCreated (gs, fs) =
  assertNewName gs { gMetadata = meta } fs
    "book with only created date" expected
  where
    meta = emptyMetadata
      { metaCreators = [Creator Nothing Nothing Nothing "Jim Jones"]
      , metaTitles = [Title Nothing Nothing Nothing "A Timeless Story"]
      , metaDates = Map.fromList [(Created, DateValue "2003")]
      }
    expected =
      ( "ordinary_book"
      , "JonesJim-ATimelessStory_2003.epub"
      )


testPubYearAnyDate :: (Globals, [Formatter]) -> TestTree
testPubYearAnyDate (gs, fs) =
  assertNewName gs { gMetadata = meta } fs
    "book with only created date" expected
  where
    meta = emptyMetadata
      { metaCreators = [Creator Nothing Nothing Nothing "Jim Jones"]
      , metaTitles = [Title Nothing Nothing Nothing "A Timeless Story"]
      , metaDates = Map.fromList [(Date, DateValue "2003")]
      }
    expected =
      ( "ordinary_book"
      , "JonesJim-ATimelessStory_2003.epub"
      )


testPubYearAnyEpub :: (Globals, [Formatter]) -> TestTree
testPubYearAnyEpub (gs, fs) =
  assertNewName gs { gMetadata = meta } fs
    "book with only the simple (date element) date" expected
  where
    meta = emptyMetadata
      { metaCreators = [Creator Nothing Nothing Nothing "Jim Jones"]
      , metaTitles = [Title Nothing Nothing Nothing "A Timeless Story"]
      , metaDates = Map.fromList [(Epub, DateValue "2003")]
      }
    expected =
      ( "ordinary_book"
      , "JonesJim-ATimelessStory_2003.epub"
      )


testPubYearAnyModified :: (Globals, [Formatter]) -> TestTree
testPubYearAnyModified (gs, fs) =
  assertNewName gs { gMetadata = meta } fs
    "book with only modified date" expected
  where
    meta = emptyMetadata
      { metaCreators = [Creator Nothing Nothing Nothing "Jim Jones"]
      , metaTitles = [Title Nothing Nothing Nothing "A Timeless Story"]
      , metaDates = Map.fromList [(Modified, DateValue "2003")]
      }
    expected =
      ( "ordinary_book"
      , "JonesJim-ATimelessStory_2003.epub"
      )


testPubYearAnyAllPresent :: (Globals, [Formatter]) -> TestTree
testPubYearAnyAllPresent (gs, fs) =
  assertNewName gs { gMetadata = meta } fs
    "book with all dates" expected
  where
    meta = emptyMetadata
      { metaCreators = [Creator Nothing Nothing Nothing "Jim Jones"]
      , metaTitles = [Title Nothing Nothing Nothing "A Timeless Story"]
      , metaDates = Map.fromList
        [ (Epub, DateValue "2001")
        , (Date, DateValue "2005")
        , (Modified, DateValue "2004")
        , (Created, DateValue "2003")
        , (Issued, DateValue "2002")
        ]
      }
    expected =
      ( "ordinary_book"
      , "JonesJim-ATimelessStory_2002.epub"
      )


testPubYearNoModified :: (Globals, [Formatter]) -> TestTree
testPubYearNoModified (gs, fs) =
  assertNewName gs { gOpts = testOpts, gMetadata = meta } fs
    "book with only modified date, --no-modified-date switch" expected
  where
    testOpts = (gOpts gs) { pubYear = NoModified }
    meta = emptyMetadata
      { metaCreators = [Creator Nothing Nothing Nothing "Jim Jones"]
      , metaTitles = [Title Nothing Nothing Nothing "A Timeless Story"]
      , metaDates = Map.fromList [ (Modified, DateValue "2004") ]
      }
    expected =
      ( "ordinary_book"
      , "JonesJim-ATimelessStory.epub"
      )


testPubYearNoDate :: (Globals, [Formatter]) -> TestTree
testPubYearNoDate (gs, fs) =
  assertNewName gs { gOpts = testOpts, gMetadata = meta } fs
    "epub book with lots of dates but we don't want"
    expected
  where
    testOpts = (gOpts gs) { pubYear = NoDate }
    meta = emptyMetadata
      { metaCreators = [Creator Nothing Nothing Nothing "Jim Jones"]
      , metaTitles = [Title Nothing Nothing Nothing "A Timeless Story"]
      , metaDates = Map.fromList
        [ (Epub, DateValue "2001")
        , (Date, DateValue "2005")
        , (Modified, DateValue "2004")
        , (Created, DateValue "2003")
        , (Issued, DateValue "2002")
        ]
      }
    expected =
      ( "ordinary_book"
      , "JonesJim-ATimelessStory.epub"
      )


testMagAeon :: (Globals, [Formatter]) -> TestTree
testMagAeon (gs, fs) =
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


testMagAEon :: (Globals, [Formatter]) -> TestTree
testMagAEon (gs, fs) =
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


testMagApexPound :: (Globals, [Formatter]) -> TestTree
testMagApexPound (gs, fs) =
  assertNewName gs { gMetadata = meta } fs
    "Apex Magazine, pound sign and issue number" expected
  where
    meta = emptyMetadata
      { metaCreators = [Creator (Just "aut") Nothing
        Nothing "Apex Authors"]
      , metaTitles = [Title Nothing Nothing Nothing
        "Apex Magazine #16"]
      }
    expected =
      ( "magApex"
      , "ApexMagazine016.epub"
      )


testMagApexIssue :: (Globals, [Formatter]) -> TestTree
testMagApexIssue (gs, fs) =
  assertNewName gs { gMetadata = meta } fs
    "Apex Magazine, Issue number" expected
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


testMagApexLong :: (Globals, [Formatter]) -> TestTree
testMagApexLong (gs, fs) =
  assertNewName gs { gMetadata = meta } fs
    "Apex Magazine, title with month year" expected
  where
    meta = emptyMetadata
      { metaCreators = [Creator (Just "aut") Nothing
        Nothing "Apex Authors"]
      , metaTitles = [Title Nothing Nothing Nothing
        "Apex Magazine: Issue 101, October 2017"]
      }
    expected =
      ( "magApex"
      , "ApexMagazine101.epub"
      )


testChallengingDestinyShort :: (Globals, [Formatter]) -> TestTree
testChallengingDestinyShort (gs, fs) =
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


testChallengingDestinyLong :: (Globals, [Formatter]) -> TestTree
testChallengingDestinyLong (gs, fs) =
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


testAnalogSingle :: (Globals, [Formatter]) -> TestTree
testAnalogSingle (gs, fs) =
  assertNewName gs { gMetadata = meta } fs "Analog, single month" expected
  where
    meta = emptyMetadata
      { metaCreators = [Creator Nothing Nothing
        Nothing "Dell Magazine Authors"]
      , metaTitles = [Title Nothing Nothing Nothing
        "Analog SFF, November 2010"]
      }
    expected =
      ( "magAnalog"
      , "AnalogSF2010-11.epub"
      )


testAnalogDouble :: (Globals, [Formatter]) -> TestTree
testAnalogDouble (gs, fs) =
  assertNewName gs { gMetadata = meta } fs "Analog, double month" expected
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


testAsimovs :: (Globals, [Formatter]) -> TestTree
testAsimovs (gs, fs) =
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

testFantasyMagazine :: (Globals, [Formatter]) -> TestTree
testFantasyMagazine (gs, fs) =
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


testFsfShort :: (Globals, [Formatter]) -> TestTree
testFsfShort (gs, fs) =
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


testFsfShortComma :: (Globals, [Formatter]) -> TestTree
testFsfShortComma (gs, fs) =
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


testFsfLong :: (Globals, [Formatter]) -> TestTree
testFsfLong (gs, fs) =
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


testFsfAmpersand :: (Globals, [Formatter]) -> TestTree
testFsfAmpersand (gs, fs) =
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


testFsfAmpersandSpaces :: (Globals, [Formatter]) -> TestTree
testFsfAmpersandSpaces (gs, fs) =
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


testFsfVeryLong :: (Globals, [Formatter]) -> TestTree
testFsfVeryLong (gs, fs) =
  assertNewName gs { gMetadata = meta } fs
    "FSF Magazine, very long name" expected
  where
    meta = emptyMetadata
      { metaCreators = [Creator Nothing Nothing
        Nothing "Spilogale Inc."]
      , metaTitles = [Title Nothing Nothing Nothing "Fantasy & Science Fiction, November/December 2017"]
      }
    expected =
      ( "magFsf"
      , "FantasyScienceFiction2017-11_12.epub"
      )


testMagFutureOrbits :: (Globals, [Formatter]) -> TestTree
testMagFutureOrbits (gs, fs) =
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


testGudShort :: (Globals, [Formatter]) -> TestTree
testGudShort (gs, fs) =
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


testGudLong :: (Globals, [Formatter]) -> TestTree
testGudLong (gs, fs) =
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


testGudVeryLong :: (Globals, [Formatter]) -> TestTree
testGudVeryLong (gs, fs) =
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


testInterzone :: (Globals, [Formatter]) -> TestTree
testInterzone (gs, fs) =
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


testInterzoneCaps :: (Globals, [Formatter]) -> TestTree
testInterzoneCaps (gs, fs) =
  assertNewName gs { gMetadata = meta } fs
    "Interzone Magazine, started in 2014" expected
  where
    meta = emptyMetadata
      { metaCreators = [Creator Nothing (Just "Interzone 249")
        Nothing "TTA Press"]
      , metaTitles = [Title Nothing Nothing Nothing 
        "INTERZONE #249 SCIENCE FICTION & FANTASY MAGAZINE (NOVâ\x80\x93DEC 2013)"]
      }
    expected =
      ( "magInterzone"
      , "InterzoneSFF249.epub"
      )


testInterzoneOldLong :: (Globals, [Formatter]) -> TestTree
testInterzoneOldLong (gs, fs) =
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
      ( "magInterzone"
      , "InterzoneSFF216.epub"
      )


testInterzoneOldShort :: (Globals, [Formatter]) -> TestTree
testInterzoneOldShort (gs, fs) =
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
      ( "magInterzone"
      , "InterzoneSFF212.epub"
      )


testNemesisShort :: (Globals, [Formatter]) -> TestTree
testNemesisShort (gs, fs) =
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


testNemesisLong :: (Globals, [Formatter]) -> TestTree
testNemesisLong (gs, fs) =
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


testMagSomethingWicked :: (Globals, [Formatter]) -> TestTree
testMagSomethingWicked (gs, fs) =
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


testMagSomethingWickedMonth :: (Globals, [Formatter]) -> TestTree
testMagSomethingWickedMonth (gs, fs) =
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


testSFBestOf :: (Globals, [Formatter]) -> TestTree
testSFBestOf (gs, fs) =
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


testBestSF :: (Globals, [Formatter]) -> TestTree
testBestSF (gs, fs) =
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


testYearsBest :: (Globals, [Formatter]) -> TestTree
testYearsBest (gs, fs) =
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


testMagBlackStatic :: (Globals, [Formatter]) -> TestTree
testMagBlackStatic (gs, fs) =
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


testRageMachineMag :: (Globals, [Formatter]) -> TestTree
testRageMachineMag (gs, fs) =
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


testEclipseMagWord :: (Globals, [Formatter]) -> TestTree
testEclipseMagWord (gs, fs) =
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


testEclipseMagNum :: (Globals, [Formatter]) -> TestTree
testEclipseMagNum (gs, fs) =
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


testBcs :: (Globals, [Formatter]) -> TestTree
testBcs (gs, fs) =
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


testBkpFileAs :: (Globals, [Formatter]) -> TestTree
testBkpFileAs (gs, fs) =
  assertNewName gs { gOpts = testOpts, gMetadata = meta } fs
    "book publisher suffix requested and present in file-as"
    expected
  where
    testOpts = (gOpts gs) { includePublisher = PublisherSwitch True }
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


testBkpText :: (Globals, [Formatter]) -> TestTree
testBkpText (gs, fs) =
  assertNewName gs { gOpts = testOpts, gMetadata = meta } fs
    "book publisher suffix requested and present in text"
    expected
  where
    testOpts = (gOpts gs) { includePublisher = PublisherSwitch True }
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


testBkpMissing :: (Globals, [Formatter]) -> TestTree
testBkpMissing (gs, fs) =
  assertNewName gs { gOpts = testOpts, gMetadata = meta } fs
    "book publisher suffix requested and not present"
    expected
  where
    testOpts = (gOpts gs) { includePublisher = PublisherSwitch True }
    meta = emptyMetadata
      { metaCreators = [Creator (Just "aut") Nothing
        Nothing "Herman Melville"]
      , metaTitles = [Title Nothing Nothing Nothing "Moby Dick"]
      }
    expected =
      ( "ordinary_book"
      , "MelvilleHerman-MobyDick.epub"
      )


testMagUniverse :: (Globals, [Formatter]) -> TestTree
testMagUniverse (gs, fs) =
  assertNewName gs { gOpts = testOpts, gMetadata = meta } fs
    "Jim Baen's Universe Magazine" expected
  where
    testOpts = (gOpts gs) { includePublisher = PublisherSwitch True }
    meta = emptyMetadata
      { metaCreators = [Creator (Just "aut") Nothing
        Nothing "Jim Baen's Universe"]
      , metaTitles = [Title Nothing Nothing Nothing
        "Jim Baen's Universe-Vol 4 Num 6"]
      }
    expected =
      ( "magGenericVolNo2"
      , "JimBaensUniverse-Vol04No06.epub"
      )


testMagClarkesworld :: (Globals, [Formatter]) -> TestTree
testMagClarkesworld (gs, fs) =
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


testLightspeedDate :: (Globals, [Formatter]) -> TestTree
testLightspeedDate (gs, fs) =
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


testLightspeedMagIssue :: (Globals, [Formatter]) -> TestTree
testLightspeedMagIssue (gs, fs) =
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


testLightspeedIssue :: (Globals, [Formatter]) -> TestTree
testLightspeedIssue (gs, fs) =
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


testLightspeedMagIssueDate :: (Globals, [Formatter]) -> TestTree
testLightspeedMagIssueDate (gs, fs) =
  assertNewName gs { gMetadata = meta } fs
    "Lightspeed Magazine, both issue and date in title" expected
  where
    meta = emptyMetadata
      { metaTitles = [Title Nothing Nothing Nothing
        "Lightspeed Magazine, Issue 62 (July 2015)"]
      }
    expected =
      ( "magLightspeed_issue"
      , "Lightspeed062.epub"
      )


testMagWeirdTales :: (Globals, [Formatter]) -> TestTree
testMagWeirdTales (gs, fs) =
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


testMagGalaxysEdge :: (Globals, [Formatter]) -> TestTree
testMagGalaxysEdge (gs, fs) =
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


testMagPlasmaFreq :: (Globals, [Formatter]) -> TestTree
testMagPlasmaFreq (gs, fs) =
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


testMagPlasmaFreqMonth :: (Globals, [Formatter]) -> TestTree
testMagPlasmaFreqMonth (gs, fs) =
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


testMagPunchinello1800s :: (Globals, [Formatter]) -> TestTree
testMagPunchinello1800s (gs, fs) =
  assertNewName gs { gMetadata = meta } fs
    "Punchinello magazine from the 1800s" expected
  where
    meta = emptyMetadata
      { metaTitles = [Title Nothing Nothing Nothing
        "Punchinello, Volume 1, No. 06, May 7, 1870"]
      }
    expected =
      ( "magGenericVolNo2"
      , "PunchinelloVol01No06.epub"
      )


testMagGenericVolNo3 :: (Globals, [Formatter]) -> TestTree
testMagGenericVolNo3 (gs, fs) =
  assertNewName gs { gMetadata = meta } fs
    "generic magazine with volume and 3-digit number" expected
  where
    meta = emptyMetadata
      { metaTitles = [Title Nothing Nothing Nothing
        "Blackwood's Edinburgh Magazine Volume 53, No. 327, January, 1843"]
      }
    expected =
      ( "magGenericVolNo3"
      , "BlackwoodsEdinburghMagazineVol53No327.epub"
      )


testMagSubjWithIssue :: (Globals, [Formatter]) -> TestTree
testMagSubjWithIssue (gs, fs) =
  assertNewName gs { gMetadata = meta } fs
    "with magazine subject and issue" expected
  where
    meta = emptyMetadata
      { metaTitles = [Title Nothing Nothing Nothing "The Dark Issue 1"]
      , metaCreators =
        [ Creator (Just "aut") (Just "Okorafor, Nnedi & Swirsky, Rachel & Slatter, Angela & Hannett, Lisa L.") Nothing
          "Nnedi Okorafor"
        , Creator (Just "aut") Nothing Nothing
          "Rachel Swirsky"
        ]
      , metaDates = Map.fromList [(Created, DateValue "2013-10-01")]
      , metaSubjects =
        [ "magazine"
        , "horror"
        , "dark fantasy"
        ]
      }
    expected =
      ( "magGenericSubjWithIssue"
      , "TheDarkMagazine01.epub"
      )


testWomensInstituteLib :: (Globals, [Formatter]) -> TestTree
testWomensInstituteLib (gs, fs) =
  assertNewName gs { gOpts = testOpts, gMetadata = meta } fs
    "Women's Institute Library" expected
  where
    testOpts = (gOpts gs) { includePublisher = PublisherSwitch True }
    meta = emptyMetadata
      { metaCreators = [Creator Nothing
        (Just "Woman's Institute of Domestic Arts and Sciences")
        Nothing "Woman's Institute of Domestic Arts and Sciences"]
      , metaTitles = [Title Nothing Nothing Nothing
        "Woman's Institute Library of Cookery\nVolume 1: Essentials of Cookery; Cereals; Bread; Hot Breads"]
      }
    expected =
      ( "nonficWomensInstituteLibrary"
      , "WomansInstituteLibraryOfCookery_Volume1_EssentialsOfCookeryCerealsBreadHotBreads.epub"
      )


testAnthologyDate :: (Globals, [Formatter]) -> TestTree
testAnthologyDate (gs, fs) =
  assertNewName gs { gMetadata = meta } fs
    "anthology with date in title" expected
  where
    meta = emptyMetadata
      { metaTitles = [Title Nothing Nothing Nothing "Flash Fiction Online - June 2015"]
      , metaCreators =
        [ Creator (Just "art") (Just "Bijelac, Dario") Nothing
          "Dario Bijelac"
        , Creator (Just "aut") (Just "Pearlman, Laura") Nothing
          "Laura Pearlman"
        ]
      , metaDates = Map.fromList [(Epub, DateValue "2015-05-30T18:47:28.000000+00:00")]
      , metaSubjects = ["anthology"]
      }
    expected =
      ( "anthology_date"
      , "FlashFictionOnline2015-06.epub"
      )


testAnthology :: (Globals, [Formatter]) -> TestTree
testAnthology (gs, fs) =
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
      , metaDates = Map.fromList [(Created, DateValue "2010-11-01")]
      , metaSubjects = ["fantasy and horror anthology"]
      }
    expected =
      ( "anthology"
      , "CreepySecrets_2010.epub"
      )


testMagLunaStation :: (Globals, [Formatter]) -> TestTree
testMagLunaStation (gs, fs) =
  assertNewName gs { gMetadata = meta } fs "Luna Station Quarterly magazine" expected
  where
    meta = emptyMetadata
      { metaTitles = [Title Nothing Nothing Nothing
        "Luna Station Quarterly - Issue 028"]
      }
    expected =
      ( "magLunaStationQuarterly"
      , "LunaStationQuarterly028.epub"
      )
