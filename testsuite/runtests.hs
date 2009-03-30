-- Copyright: 2007, 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Main
   where

import Test.HUnit
import Test.BookName.Format.AuthorBasic
import Test.BookName.Format.MagDell
import Test.BookName.Format.MagNemesis


main :: IO Counts
main = runTestTT tests


tests :: Test
tests = TestList
   [ testAuthorBasic
   , testMagDell
   , testMagNemesis
   ]
