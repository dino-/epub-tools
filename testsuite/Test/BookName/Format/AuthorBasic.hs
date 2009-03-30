-- Copyright: 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Test.BookName.Format.AuthorBasic
   ( testAuthorBasic )
   where

import Test.HUnit ( Test (..), assertEqual )
import Test.HUnit.Base ( Assertion )

import BookName.Extract
import BookName.Format.AuthorBasic
import BookName.Util


assertNewName :: String -> [String] -> String -> Assertion
assertNewName desc meta expected = do
   let fields = parseMeta "foo" $ unlines meta
   result <- runBN $ fmtAuthorBasic fields
   let actual = either id id result
   assertEqual desc expected actual


testAuthorBasic :: Test
testAuthorBasic = TestCase $
   assertNewName "testAuthorBasic" lrfMeta expected
   where
      lrfMeta =
         [ "Author: Herman Melville"
         , "Title: Moby Dick"
         , "FreeText: 1851 Herman Melville"
         ]
      expected = "MelvilleHerman-MobyDick_1851.lrf"
