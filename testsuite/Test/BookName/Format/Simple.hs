-- Copyright: 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Test.BookName.Format.Simple (
   testSimpleAll
)
   where

import Test.HUnit ( Test (..), assertEqual )

import BookName.Extract
import BookName.Format.Simple
import BookName.Util


testSimpleAll :: Test
testSimpleAll = TestList
   [ testSimpleWithThe
   ]


testSimpleWithThe :: Test
testSimpleWithThe = TestCase $ do
   let lrfMeta = unlines
         [ "Author: Kate Wilhelm"
         , "FreeText: 1974 Kate Wilhelm"
         , "Title: The Hounds"
         ]
   let fields = parseMeta "foo" lrfMeta
   result <- runBN $ formatSimple fields
   let actual = either id id result
   assertEqual "testSimpleWithThe"
      "WilhelmKate-Hounds_1974.lrf" actual
