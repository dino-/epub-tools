-- Copyright: 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Test.BookName.Format.Simple (
   testSimpleAll
)
   where

import Test.HUnit ( Test (..), assertEqual )
import Test.HUnit.Base ( Assertion )

import BookName.Extract
import BookName.Format.Simple
import BookName.Util


assertNewName :: String -> [String] -> String -> Assertion
assertNewName desc meta expected = do
   let fields = parseMeta "foo" $ unlines meta
   result <- runBN $ formatSimple fields
   let actual = either id id result
   assertEqual desc expected actual


testSimpleAll :: Test
testSimpleAll = TestList
   [ testNoAuthor
   , testNoTitle
   , testAllPunctuation
   , testAuthorDouble
   , testAuthorThird
   , testAuthorJr
   , testAuthorSt
   , testWithThe
   ]


testNoAuthor :: Test
testNoAuthor = TestCase $
   assertNewName "testNoAuthor" lrfMeta expected
   where
      lrfMeta =
         [ "FreeText: Doesn't matter"
         , "Title: Insignificant"
         ]
      expected = "[ERROR missing key: Author]"


testNoTitle :: Test
testNoTitle = TestCase $
   assertNewName "testNoTitle" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Nobody McCrankypants"
         , "FreeText: Doesn't matter"
         ]
      expected = "[ERROR missing key: Title]"


testAllPunctuation :: Test
testAllPunctuation = TestCase $
   assertNewName "testAllPunctuation" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Dino Morelli"
         , "FreeText: 2008 Dino Morelli"
         , "Title: The *crazy*: Sand-box. Of Smedley's discontent, fear & Malnourishment? (Maybe not!); [Part #2]"
         ]
      expected = "MorelliDino-CrazySandBoxOfSmedleysDiscontentFearAndMalnourishmentMaybeNot_Part2_2008.lrf"


testAuthorDouble :: Test
testAuthorDouble = TestCase $
   assertNewName "testAuthorDouble" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Ren Hoek and Stimpson J. Cat"
         , "FreeText: 1993 Spumco"
         , "Title: The Ask Dr. Stupid Compendium"
         ]
      expected =
         "Hoek_Cat-AskDrStupidCompendium_1993.lrf"


testAuthorThird :: Test
testAuthorThird = TestCase $
   assertNewName "testAuthorThird" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Thurston Howell III"
         , "FreeText: 1974, SS Minnow Unltd."
         , "Title: My Life as a Deserted Island Playboy"
         ]
      expected =
         "HowellIIIThurston-MyLifeAsADesertedIslandPlayboy_1974.lrf"


testAuthorJr :: Test
testAuthorJr = TestCase $
   assertNewName "testAuthorJr" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Jonas Venture Jr."
         , "FreeText: 2008, Venture Industries"
         , "Title: The Venture Legacy"
         ]
      expected =
         "VentureJrJonas-VentureLegacy_2008.lrf"


testAuthorSt :: Test
testAuthorSt = TestCase $
   assertNewName "testAuthorSt" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Jennifer St. Clair"
         , "FreeText: 2004, Jennifer St. Clair"
         , "Title: Budget Cuts"
         ]
      expected = "StClairJennifer-BudgetCuts_2004.lrf"


testWithThe :: Test
testWithThe = TestCase $
   assertNewName "testWithThe" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Kate Wilhelm"
         , "FreeText: 1974 Kate Wilhelm"
         , "Title: The Hounds"
         ]
      expected = "WilhelmKate-Hounds_1974.lrf"
