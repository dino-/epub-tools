-- Copyright: 2007, 2008 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Main
   where

import Test.HUnit
import Test.BookName.Format.Anonymous
import Test.BookName.Format.AuthorBasic
import Test.BookName.Format.AuthorDouble
import Test.BookName.Format.AuthorThird
import Test.BookName.Format.General
import Test.BookName.Format.MagAeon
import Test.BookName.Format.MagApex
import Test.BookName.Format.MagChallengingDestiny
import Test.BookName.Format.MagDell
import Test.BookName.Format.MagFsf
import Test.BookName.Format.MagFutureOrbits
import Test.BookName.Format.MagGud
import Test.BookName.Format.MagInterzone
import Test.BookName.Format.MagNemesis
import Test.BookName.Format.MagSomethingWicked
import Test.BookName.Format.SFBestOf


main :: IO Counts
main = runTestTT tests


tests :: Test
tests = TestList
   [ testAnonymous
   , testAuthorBasic
   , testAuthorDouble
   , testAuthorThird
   , testGeneral
   , testMagAeon
   , testMagApex
   , testMagChallengingDestiny
   , testMagDell
   , testMagFsf
   , testMagFutureOrbits
   , testMagGud
   , testMagInterzone
   , testMagNemesis
   , testMagSomethingWicked
   , testSFBestOf
   ]
